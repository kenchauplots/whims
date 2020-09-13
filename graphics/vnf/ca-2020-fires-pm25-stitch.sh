# Performs final video editing for CA fires videos
# Script requires FFMPEG compiled from source to include ffmpeg-gl-transition
# see: https://github.com/transitive-bullshit/ffmpeg-gl-transition

# check & remove existing videos if there's any
[ -e ca-2020-fires-slow.mp4 ] && rm ca-2020-fires-slow.mp4;
[ -e ca-2020-fires-pad.mp4 ] && rm ca-2020-fires-pad.mp4;
[ -e ca-2020-fires-fade.mp4 ] && rm ca-2020-fires-fade.mp4;
[ -e ca-2020-fires.mp4 ] && rm ca-2020-fires.mp4;
[ -e ca-2020-pm25-slow.mp4 ] && rm ca-2020-pm25-slow.mp4;
[ -e ca-2020-pm25-pad.mp4 ] && rm ca-2020-pm25-pad.mp4;
[ -e ca-2020-pm25-fade.mp4 ] && rm ca-2020-pm25-fade.mp4;
[ -e ca-2020-pm25.mp4 ] && rm ca-2020-pm25.mp4;

# slow down daily video
~/ffmpeg/ffmpeg -i ca-2020-fires-daily.mp4 -vf "setpts=1.25*PTS" -y ca-2020-fires-slow.mp4;
# add extra time to last daily frame
~/ffmpeg/ffmpeg -i ca-2020-fires-slow.mp4 -vf tpad=stop_mode=clone:stop_duration=6 -y ca-2020-fires-pad.mp4;
# crossfade with total video
~/ffmpeg/ffmpeg -i ca-2020-fires-pad.mp4 -i ca-2020-fires-end.mp4 -filter_complex "gltransition=duration=1:offset=21" -y ca-2020-fires-fade.mp4;
# crop off extra white bits
~/ffmpeg/ffmpeg -i ca-2020-fires-fade.mp4 -vf "crop=908:998" -y ca-2020-fires.mp4;

# slow down daily video
~/ffmpeg/ffmpeg -i ca-2020-pm25-daily.mp4 -vf "setpts=1.25*PTS" -y ca-2020-pm25-slow.mp4;
# add extra time to last daily frame
~/ffmpeg/ffmpeg -i ca-2020-pm25-slow.mp4 -vf tpad=stop_mode=clone:stop_duration=6 -y ca-2020-pm25-pad.mp4;
# crossfade with last day video
~/ffmpeg/ffmpeg -i ca-2020-pm25-pad.mp4 -i ca-2020-pm25-end.mp4 -filter_complex "gltransition=duration=1:offset=21" -y ca-2020-pm25-fade.mp4;
# crop off extra white bits
~/ffmpeg/ffmpeg -i ca-2020-pm25-fade.mp4 -vf "crop=894:998" -y ca-2020-pm25.mp4;

# horizontal stack the 2 videos
# -pix_fmt yuv420p to be web-compliant
~/ffmpeg/ffmpeg -i ca-2020-fires.mp4 -i ca-2020-pm25.mp4 -filter_complex hstack=inputs=2 -pix_fmt yuv420p -y ca-2020-fires-pm25.mp4

# delete videos
rm ca-2020-fires-slow.mp4;
rm ca-2020-fires-pad.mp4;
rm ca-2020-fires-fade.mp4;
rm ca-2020-fires.mp4;
rm ca-2020-pm25-slow.mp4;
rm ca-2020-pm25-pad.mp4;
rm ca-2020-pm25-fade.mp4;
rm ca-2020-pm25.mp4;

# open final video-final
# xdg-open ca-2020-fires-pm25.mp4;
# ffmpeg -i ca-2020-fires-pm25.mp4 -vf "select=eq(n\,380)" -vframes 1 -y ca-2020-fires-pm25-demo.png;