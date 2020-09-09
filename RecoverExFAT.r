# RecoverExFAT.r
# Tool to recover all data from a reformatted exFAT partition (e.g. if you overwrite the partition table)
# Probably only works correctly under Linux, patches welcome
# Run as root to be able to read raw data, else pass an image for DiskToRecover
# Copyright (c) Dainius Masiliunas, 2020
# Licensed under GPLv3+

## Global variables

# The input disk that we want to recover
DiskToRecover = "/dev/sdb"
#DiskToRecover = "diskhead.img"
# Number of bytes per block
BlockSize = 512
# exFAT cluster size.
# You can figure it out by doing some `dd if=DiskToRecover of=header.bin count=16384` and
# looking at where useful information starts after a whole lot of 0s. Usually it's at cluster boundaries, i.e. at block 2048, 4096, etc.
ClusterSize = BlockSize*2048
# Start of the exFAT partition.
# This is important to map cluster numbers to their block locations.
# You can figure it out by looking at the dump above and seeing where the first file is located,
# compared with the recovered FAT table in the first pass.
StartOffset = BlockSize*4096
# The first cluster that has data (i.e. the root folder cluster), in cluster number.
# We traverse the tree from there.
StartCluster = 4
# How large is the root folder. This asumes you have <64 files in it. Increase the number if the root does not fit into a single cluster.
StartClusterSize = ClusterSize

# Where to write the first pass FAT CSV file
#FATFile = "FAT.csv"
# Where to write second-pass recovered files
OutputDir = "/run/media/dainius/Recovered/R"
# What size chunks to move (to save memory), must be below 2^31
MaxChunkSize = 2^31-1

## Functions

# Problem with R is that it will break as soon as it sees a 00, which is every other character in a Unicode string.
# We don't want to just remove the 00s because we may have Chinese characters in filenames etc., and that
# would make it display as a combination of two other characters. So parse strings character-by-character.
#' @param RawString The raw string that has been read by readBin.
readUTF8Char = function(RawString)
{
    # Append an ff fe for iconv, but also skip padding
    AppendUnicodeMark = function(RawChar){if (identical(RawChar, as.raw(c(0x0, 0x0)))) return(NULL) else return(c(as.raw(c(0xff, 0xfe)), RawChar))}
    
    CharList = split(RawString, rep(1:(length(RawString)/2), each=2))
    CharList = lapply(CharList, AppendUnicodeMark)
    paste0(na.omit(iconv(CharList, from="Unicode")), collapse="")
}

rawToUInt64 = function(RawNumber)
{
    sum(2^.subset(0:63, as.logical(rawToBits(RawNumber))))
}

# Check if the record matches a file record
IsFileRecord = function(Record)
{
    if (Record[1] != 0x85 ||
        !all(Record[7:8] == 0) ||
        !all(Record[26:32] == 0)) return(FALSE)
    return(TRUE)        
}

# Read a file record and parse it to a FAT entry.
# The connection needs to be at the right position.
ParseFileRecord = function(conn)
{
    stopifnot(readBin(conn, "raw", 1) == 0x85)
    readBin(conn, "raw", 5) # Checksums and attributes
    readBin(conn, "raw", 2) == 0 # Reserved
    # TODO: Better handling of the cases where the integer is unsigned (all integers here are!)
    CreateTime = readBin(conn, "integer", 1, 4) # These are bitmaps! TODO: parse into POSIXct
    ModifiedTime = readBin(conn, "integer", 1, 4)
    AccessTime = readBin(conn, "integer", 1, 4)
    readBin(conn, "raw", 5) # Timezones etc.
    readBin(conn, "raw", 7) == 0 # Reserved

    # File location
    readBin(conn, "raw", 1) == 0xC0
    seek(conn, 19, "current")
    StartingCluster = readBin(conn, "integer", 1, 4) # In clusters since the start of the partition
    #FileLength = readBin(conn, "integer", 1, 8, FALSE) # In bytes
    FileLengthRaw = readBin(conn, "raw", 8)
    FileLength = rawToUInt64(FileLengthRaw)

    # File name
    FileName = ""
    while (TRUE)
    {
        if (readBin(conn, "raw", 1) != 0xC1)
        {
            # This is something else - stop reading strings and go back!
            seek(conn, -1, "current")
            break
        }
        seek(conn, 1, "current") # Flags
        FileNamePart = readUTF8Char(readBin(conn, "raw", 30))
        FileName = paste0(FileName, FileNamePart)
    }

    return(data.frame(FileName, StartingCluster,
        StartingByte=ClusterSize*StartingCluster+StartOffset, FileLength,
        CreateTime, ModifiedTime, AccessTime))
}

# Search the cluster for records.
# Records are 32 bytes in size and have a particular signature.
# Find a file name record to build up a FAT.
# ReadFor can be used to stop early to speed things up.
ParseMetadataCluster = function(conn, ClusterNumber, ReadFor=ClusterSize)
{
    # Go to the requested cluster
    StartPosition = StartOffset + ClusterSize*ClusterNumber
    seek(conn, StartPosition)
    FAT=NULL
    # Read until the requested end
    while (seek(conn) < StartPosition + ReadFor)
    {
        Record = readBin(conn, "raw", 32)
        if (IsFileRecord(Record))
        {
            seek(conn, -32, "current")
            FAT = rbind(FAT, ParseFileRecord(conn))
        }
    }
    return(FAT)
}

# Parse an arbitrary cluster recursively.
# Either it is a metadata cluster, or a data cluster.
# If it is metadata, pass it to ParseMetadataCluster.
# If it's data, save the file to disk.
#' @param FATEntry A named vector, list or data.frame with a single row that contains
#' columns FileName, StartingCluster and FileLength.
#' @param Folder The path that this cluster represents.
#' @param ProbeRecords Number of records to read before assuming the cluster is data and not metadata.
ParseCluster = function(conn, FATEntry, Folder="", ProbeRecords = 5)
{
    SaveFolder = file.path(OutputDir, Folder)
    if (!dir.exists(SaveFolder))
        dir.create(SaveFolder)
    SaveFile = file.path(SaveFolder, FATEntry[["FileName"]])
    if (!dir.exists(SaveFile) && file.exists(SaveFile))
    {
        print(paste("File already in output directory, skipping:", SaveFile))
        return()
    }
    
    StartPosition = StartOffset + ClusterSize*FATEntry[["StartingCluster"]]
    seek(conn, StartPosition)
    # Read first 5 records to make sure that the cluster is a data cluster and not a metadata cluster
    while (seek(conn) < StartPosition + ProbeRecords * 32)
    {
        Record = readBin(conn, "raw", 32)
        if (IsFileRecord(Record))
        {
            # The cluster we are parsing is actually a folder! Let's parse it as a folder.
            seek(conn, -32, "current")
            FolderPath = file.path(Folder, FATEntry[["FileName"]])
            print(paste("Traversing folder:", FolderPath))
            
            FAT = ParseMetadataCluster(conn, FATEntry[["StartingCluster"]], FATEntry[["FileLength"]])
            print(FAT)
            for (i in 1:nrow(FAT))
                ParseCluster(conn, FAT[i,], Folder=FolderPath, ProbeRecords=ProbeRecords)
            return()
        }
    }
    # Didn't find any metadata records, so assume it's a data file. Save it!
    
    print(paste("Saving file:", SaveFile))
    FileHandle = file(SaveFile, "ab")
    # Is the file > 2 GB? Then we need multiple steps.
    BytesRemaining = FATEntry[["FileLength"]]
    while (BytesRemaining > 0)
    {
        ChunkSize = min(BytesRemaining, MaxChunkSize)
        seek(conn, StartPosition)
        FileData = readBin(conn, "raw", ChunkSize)
        writeBin(FileData, FileHandle)
        BytesRemaining = BytesRemaining - ChunkSize
        StartPosition = StartPosition + ChunkSize
    }
    close(FileHandle)
}

#write.csv(FAT, FATFile, fileEncoding="UTF-8")

## Main body

Disk = file(DiskToRecover, "rb", raw=TRUE)

options(stringsAsFactors=FALSE, warn=1)

# Start from the root of the filesystem and recurse into directories from there!
ParseCluster(Disk, list(FileName="", StartingCluster=StartCluster, FileLength=StartClusterSize))
# If your root directory is also damaged, you can run the above on any directory, passing FileName set to the name of the directory.

close(Disk)
