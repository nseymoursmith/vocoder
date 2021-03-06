(ns vocoder.vocoder-example
  (:use [overtone.live]
        [vocoder.core]))
(vocoder)
;;This synth works well with the vocoder presets
(definst pad2
  [note 60 t 10 amt 0.3 amp 0.1 a 0.4 d 0.5 s 0.8 r 2 gate 1]
  (let [freq   (midicps note)
        lfo    (+ 2 (* 0.01 (sin-osc:kr 5 (rand 1.5))))
        src    (apply + (saw [freq (* freq lfo)]))
        env    (env-gen (adsr a d s r) gate :action FREE)
        f-env  (x-line:kr 0.001 4 t)
        src    (* env src)
        signal (rlpf src (+ (* 0.3 freq) (* f-env 2 freq)) 0.5)
        k      (/ (* 4 amt) (- 1 amt))
        dist   (clip2 (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
                      0.03)
        snd    (* amp dist (line:kr 1 0 t))]
    src))

;;Attach vocoder to synth and start midi player
(do
  (def voco (inst-fx! pad2 vocoder))
  (def voc-player (midi-poly-player pad2))
)

;;Or attach vocoder to synth and just play a note
;;and play using ctl
(do
  (def voco (inst-fx! pad2 vocoder))
  (pad2)
)
(ctl pad2 :note 50)

;;Stop midi, kill fx and synths
(do
  (clear-fx pad2)
  (midi-player-stop voc-player)
  (stop)
)

;;recording
(recording-start "~/Desktop/mic-check-24.wav")
(recording-stop)

;;preset play
(do 
  (ctl voco :comp-thresh 0.01)
  (ctl voco :voc-gain 1)
  (ctl voco :src-gain 1)
  (ctl voco :comp-gain 1.0)
  (ctl voco :fsib 8000)
  (ctl voco :sib-gain 0.1)
  (ctl voco :rq-voc 0.32)
  (ctl voco :rq-synth 0.32)
  (ctl voco :distance 10)
  (ctl voco :gain 140.0))
