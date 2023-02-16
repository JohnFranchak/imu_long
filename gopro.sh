#!/bin/bash
dir=$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
echo pwd
rm filelist.txt
touch filelist.txt
yourfilenames=`ls GX*`
for eachfile in $yourfilenames
do
   echo $eachfile
   printf "file '%s'\n" $eachfile >> filelist.txt
done
echo Concatenating Videos
/usr/local/bin/ffmpeg -y -f concat -i filelist.txt -c copy -map 0:0 -map 0:1 -map 0:3 cat.mp4
echo Compressing Videos
/usr/local/bin/ffmpeg -i cat.mp4 -c:v h264_videotoolbox -q:v 40 compressed.mp4

rm cat.mp4
rm filelist.txt

/usr/local/bin/ffmpeg -i compressed.mp4 -t 01:30:00 -c copy compressed_pt1.mp4 \
-ss 01:30:00 -c copy compressed_pt2.mp4

rm compressed.mp4