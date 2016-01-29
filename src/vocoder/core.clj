(ns vocoder.core
  (:use [overtone.live]
        [overtone.inst.synth]))

(defn- nbpf
  ; n-stage recursive band-pass filter
  [input freq rq stages]
  (loop [n stages signal input]
    (if (zero? n) signal
        (recur (dec n) (bpf signal freq rq)))))

(defsynth vocoder
  [bus 0
   mic-input 8
   fmin 20.0
   fmax 10000.0
   nbands 9
   stages 2
   rq-voc 0.32
   rq-synth 0.32
   distance 10.0
   voc-gain 1.0
   src-gain 1.0
   comp-gain 1.0
   fsib 8000.0
   sib-gain 0.001
   gain 140.0]
  (let [; - sources - ;
        src (in bus)
        input (in mic-input)

        ; - mic frequency band amplitudes -
        voc (* voc-gain input)
        voc-level (amplitude:kr voc 0.1 0.1)
        fmin* (. fmin value)
        fmax* (. fmax value)
        nbands* (. nbands value)
        base (Math/pow (/ fmax* fmin*) (/ nbands*))
        n (range nbands*)
        band-freqs (map #(* fmin* (Math/pow base (+ % 1))) n)
        stages* (. stages value)
        voc-bands (map #(nbpf voc % rq-voc stages*) band-freqs)
        ;; voc-bands (map #(bpf voc % rq-voc) band-freqs)
        ;; voc-bands (map (fn [band centre] (bpf band centre rq-synth)) 
        ;;                voc-bands band-freqs)
        taus (map #(/ distance %) band-freqs)
        voc-amps (map (fn [band tau] (amplitude band tau tau)) 
                      voc-bands taus)
;;        voc-amps (map #(/ % voc-level) voc-amps)
        

        ; - extract and mix src bands -
        src (* src-gain src)
        src-level (amplitude:kr src 0.1 0.1)
        src-bands (map #(nbpf src % rq-synth stages*) band-freqs)
        ;; src-bands (map (fn [band centre] (bpf band centre rq-synth)) 
        ;;                src-bands band-freqs)
        src-voc-mix (map * src-bands voc-amps)
        composited (* comp-gain (sum src-voc-mix))

        ; - white-noise synthesis for sibilance - ;
        a (buffer 2048)
        b (buffer 2048)
        noise-source (white-noise)
        formed (pv-mul (fft a input) (fft b noise-source))
        sib-synth (ifft formed)
        sibilance (hpf sib-synth fsib)
        sibilance (* sib-gain src-level sibilance)

        output (sum [composited sibilance])
        ]
    (replace-out bus (* gain output))))

(do
  (def voco (inst-fx! pad2 vocoder))
  (def voc-player (midi-poly-player pad2))
)
;;Stop midi-controlled vocoder
(do
  (clear-fx pad2)
  (midi-player-stop voc-player)
  (stop)
)

(recording-start "~/Desktop/mic-check-24.wav")
(recording-stop)

;;preset play
(do 
  (ctl voco :voc-gain 1)
  (ctl voco :src-gain 1)
  (ctl voco :comp-gain 1.0)
  (ctl voco :fsib 8000)
  (ctl voco :sib-gain 0.001)
  (ctl voco :rq-voc 0.32)
  (ctl voco :rq-synth 0.32)
  (ctl voco :distance 10)
  (ctl voco :gain 140.0))

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

(defsynth bizzle [out-bus 10 amp 0.5]
  (out out-bus
       (* amp
          (+ (* (decay2 (* (impulse 10 0)
                           (+ (* (lf-saw:kr 0.3 0) -0.3) 0.3))
                        0.001)
                0.3)
             (apply + (pulse [80 81]))))))

