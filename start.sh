./stop.sh
# start the slub sync deamon, listening on port 4000
syncup 4000 osc.udp://127.0.0.1:4444 & 
fluxus -x sb.scm
