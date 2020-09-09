# RecoverExFAT
A tool to completely recover data from an exFAT partition when the headers are overwritten

The tool works by reading clusters with metadata about the files and file names, and recursively traversing the directory tree.
Thus it works better than PhotoRec in that it recovers every single file and all of the filenames (and in the future also the timestamps; patches welcome).

The tool is ideal in the case that your exFAT partition header got overwritten, e.g. you accidentally overwrote the partition table and also created a new partition on top of it before realising the mistake.
In those cases testdisk will only find the new partition and not the old one, as the metadata is also misaligned.
In addition, the tool works even if more of the (meta)data got overwritten, by parsing individual directory trees and recovering them.
Note that if a data block got overwritten, you may still get a corrupt file, but it will have the correct metadata.

To use the tool, you need to look into the `RecoverExFAT.r` file and adjust some parameters based on your situation.
It may involve some extra knowledge from the user's part at the moment, but for the most part running the tool on some arbitrary clusters at the top of the disk should be enough for it to start the recursive recovery process.
