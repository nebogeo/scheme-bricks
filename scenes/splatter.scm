((#(0.7394179701805115 0.0 0.0 0.0 0.0 0.7394179701805115 0.0 0.0 0.0 0.0 0.7394179701805115 0.0 -25.294382095336914 18.38405418395996 0.0 1.0) "(define (zop time clock zap) (play time (crush (* 1 (adsr 0 (pick (list 0.02 0.05 0.1) clock) (pick (list 0 0.3 0 0 0.1 0) zap) 1) (pick (list (mooghp (pink 10) (sine 0.1) 1) (white 3) (sine (* 200 (adsr 0 0.1 0.3 1)))) clock)) (pick (list 0.02 0.1 3 2 10) clock) 10)) (if (modeq? clock 16) (in time 0.5 zopp 0 (+ zap 1)) (in time (pick (list 0.25) clock) zop (+ clock 1) zap)))") (#(0.826685905456543 0.0 0.0 0.0 0.0 0.826685905456543 0.0 0.0 0.0 0.0 0.826685905456543 0.0 4.558143615722656 14.500986099243164 0.0 1.0) "(bootstrap (lambda () zop))") (#(0.826685905456543 0.0 0.0 0.0 0.0 0.826685905456543 0.0 0.0 0.0 0.0 0.826685905456543 0.0 4.70802640914917 -7.540102958679199 0.0 1.0) "(reset)") (#(0.6497272849082947 0.0 0.0 0.0 0.0 0.6497272849082947 0.0 0.0 0.0 0.0 0.6497272849082947 0.0 -5.900012493133545 30.991817474365234 0.0 1.0) "(define (zopp time clock zap) (play time (* (adsr 0 0.2 0.1 1) (sine (+ (note (* 3 (modulob zap 17))) (* (adsr (pick (list 0 0 0.3 0) clock) 0.03 0.3 2) 210 (sine (note (* 30 (modulor zap 7)))))))) 0.6) (if (modeq? clock 8) (in time 0.5 zoppp clock zap) (in time (pick (list 1.5 1.5 0.25) clock) zopp (+ clock 1) zap)))") (#(0.826685905456543 0.0 0.0 0.0 0.0 0.826685905456543 0.0 0.0 0.0 0.0 0.826685905456543 0.0 4.804286956787109 4.350552558898926 0.0 1.0) "clock") (#(0.6366130709648132 0.0 0.0 0.0 0.0 0.6366130709648132 0.0 0.0 0.0 0.0 0.6366130709648132 0.0 12.854046821594238 38.42527389526367 0.0 1.0) "(define (zoppp time clock zap) (play time (mooghp (* (adsr 0 0.05 0 1) (+ (saw (note (* (modulob clock 13) 10))) (saw (note (* (modulob zap 5) 12))))) (* (pick (list 0 0.3 0.5 1 0.8) clock) (adsr 0 0.1 0 0)) 0.4) (sin clock)) (when (< clock 40) (in time 0.25 zoppp (+ clock 1) zap)))") (#(0.602653980255127 0.0 0.0 0.0 0.0 0.602653980255127 0.0 0.0 0.0 0.0 0.602653980255127 0.0 6.116777420043945 -13.004716873168945 0.0 1.0) "(+)") (#(0.7588688731193542 0.0 0.0 0.0 0.0 0.7588688731193542 0.0 0.0 0.0 0.0 0.7588688731193542 0.0 -13.01314926147461 -2.838911533355713 0.0 1.0) "(pick clock)") (#(0.6497272849082947 0.0 0.0 0.0 0.0 0.6497272849082947 0.0 0.0 0.0 0.0 0.6497272849082947 0.0 5.880367755889893 -2.4610347747802734 0.0 1.0) "(echo)") (#(0.7394179701805115 0.0 0.0 0.0 0.0 0.7394179701805115 0.0 0.0 0.0 0.0 0.7394179701805115 0.0 -14.926963806152344 3.137336254119873 0.0 1.0) "clock") (#(0.7394179701805115 0.0 0.0 0.0 0.0 0.7394179701805115 0.0 0.0 0.0 0.0 0.7394179701805115 0.0 -13.891815185546875 13.402853012084961 0.0 1.0) "(saw (* 2000 (adsr 0 0.2 0.3 1)))") (#(0.6366130709648132 0.0 0.0 0.0 0.0 0.6366130709648132 0.0 0.0 0.0 0.0 0.6366130709648132 0.0 23.772884368896484 3.9324898719787598 0.0 1.0) "(+ 19 (* 440 (sine 0.2)))"))