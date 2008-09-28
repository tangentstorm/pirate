
(import sys 
	time
	pygame)

(if (!= 2 (len sys.argv))
    (: (sys.stdout.write (% "usage: %s TIMEOUT"
			    (@ 0 sys.argv)))
       (sys.exit)))

(:= WAVFILE "Nice_Not-NEO_Soun-1374.wav")
(:= WAIT (int (@ 1 sys.argv)))

(def main ()
     (pygame.mixer.init)
     (pygame.mixer.music.load WAVFILE)
     (pygame.mixer.music.set_volume 0.25)
     (time.sleep WAIT)
     (pygame.mixer.music.play)
     (time.sleep 2))

(main)
