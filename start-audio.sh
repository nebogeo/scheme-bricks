./stop-audio.sh
xset s 0
hdparm -S 0 /dev/sda7
hdparm -S 0 /dev/sda5
jackd -d alsa -r 44100 -p 1024 &
sleep 1
fluxa -jackports system:playback_1 system:playback_2 &
