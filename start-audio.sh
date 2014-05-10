./stop-audio.sh
killall pulseaudio
xset s 0
hdparm -S 0 /dev/sda7
hdparm -S 0 /dev/sda5
jackd -R -d alsa -r 44100 -p 1024 &
sleep 1
fluxa -jackports system:playback_1 system:playback_2 &
