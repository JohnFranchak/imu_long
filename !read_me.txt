Participant numbers will start sequentially from 100.

For each participant, there will be a subfolder for each session:
1 = 4/11
2 = 5/12
3 = 6/13
4 = 7/14

Within each session folder, there will be a log file (log_XXX_Y.csv) and subfolders for:
coding (datavyu files)
imu (all imu data)
lena (reports and ITS downloaded from lena SP)
videos_converted (codeable videos)
videos_raw (raw insta360 video files)

The contents of each folder should be as follows
(where XXX is the participant number, Y is the session):

coding - imul_XXX_Y_part1.opf, imul_XXX_Y_part2.opf
imu - subfolders for:
	right_ankle, left_ankle, right_hip, left_hip, and caregiver
	infant subfolders will have accel.csv and gyro.csv
	caregiver folder will have hip.csv and wrist.csv
lena - raw audio & data downloaded from LENA SP, TBD
videos_converted - imul_XXX_Y_part1.mp4, imul_XXX_Y_part2.mp4
videos_raw - keep raw insta360 videos with their original names
