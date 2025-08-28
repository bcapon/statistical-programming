# Set working directory to where your frames are
setwd("D:/University of Edinburgh Masters/Sem 1/statistical-programming/projects/project2/frames")

files <- list.files(pattern = "^frame_\\d+\\.png$")
for (file in files) {
  num <- as.numeric(gsub("frame_(\\d+)\\.png", "\\1", file))
  new_name <- sprintf("frame_%03d.png", num)
  file.rename(file, new_name)
}

library(av)
files <- list.files(pattern = "^frame_\\d{3}\\.png$")
# Create video with specified fps
av_encode_video(
  input = files,
  framerate = 2,                        # Specify your desired FPS
  output = "output_video.mp4"
)
