(ns vocoder.core
  (:use [overtone.live]
        [overtone.inst.synth]))

(defsynth vocoder
  [bus 0
   mic-input 8
   fmin 20.0
   fmax 20000.0
   nbands 10
   rq-voc 0.65
   rq-synth 0.65
   distance 1.0
   voc-gain 1.0
   src-gain 1.0
   comp-gain 1.0
   fsib 8000.0
   sib-gain 1.0
   gain 1.0]
  (let [; - sources - ;
        src (in bus)
        input (in mic-input)

        ; - mic frequency band amplitudes -
        exp-base (pow (/ fmax fmin) (/ nbands))
        band-freqs (map #(* fmin (pow exp-base (+ % 1))) 
                        (range (. nbands value)))
        voc (* voc-gain input)
        voc-bands (map #(bpf voc % rq-voc) band-freqs)
        voc-amps (map (fn [band center] 
                        (let [tau (* distance (/ center))]
                          (amplitude band tau tau)))
                      voc-bands band-freqs)

        ; - extract and mix src bands -
        src (* src-gain src)
        src-bands (map #(bpf src % rq-synth) band-freqs)
        src-voc-mix (map * src-bands voc-amps)
        composited (* comp-gain (sum src-voc-mix))

        ; - white-noise synthesis for sibilance - ;
        a (buffer 2048)
        b (buffer 2048)
        noise-source (white-noise)
        formed (pv-mul (fft a input) (fft b noise-source))
        sib-synth (ifft formed)
        sibilance (hpf sib-synth fsib)
        src-level (amplitude:kr src)
        sibilance (* sib-gain src-level sibilance)

        output (sum [composited sibilance])]
    (replace-out bus (* gain output))))

(definst cs80lead-midi
  [note 69
   amp 0.5
   gate 1
   att 0.75
   decay 0.5
   sus 0.8
   rel 1.0
   fatt 0.75
   fdecay 0.5
   fsus 0.8
   frel 1.0
   cutoff 200
   dtune 0.002
   vibrate 4
   vibdepth 0.015
   ratio 1
   cbus 1
   freq-lag 0.1]
  (let [freq (midicps note)
        freq (lag freq freq-lag)
        cuttoff (in:kr cbus)
        env     (env-gen (adsr att decay sus rel) gate :action FREE)
        fenv    (env-gen (adsr fatt fdecay fsus frel 2) gate)

        vib     (+ 1 (lin-lin:kr (sin-osc:kr vibrate) -1 1 (- vibdepth) vibdepth))

        freq    (* freq vib)
        sig     (mix (* env amp (saw [freq (* freq (+ dtune 1))])))]
    sig))

;;Start midi-controlled vocoder with presets
(do
  (def voco (inst-fx! cs80lead-midi vocoder))
  (def voc-player (midi-poly-player cs80lead-midi))
)

;;Stop midi-controlled vocoder
(do
  (midi-player-stop voc-player)
  (clear-fx cs80lead-midi)
  (stop))

;;preset for messing around
(do 
  (ctl cs80lead-midi :note 50)
  (ctl cs80lead-midi :vibdepth 0.015)
  (ctl cs80lead-midi :vibrate 8)
  (ctl cs80lead-midi :sus 1)
  (ctl voco :voc-band-gain 10)
  (ctl voco :src-band-gain 10)
  (ctl voco :comp-gain 1.001)
  (ctl voco :fsib 8000)
  (ctl voco :sib-gain 1.0010)
  (ctl voco :rq-voc 0.15)
  (ctl voco :rq-synth 0.15)
  (ctl voco :distance 100)
  (ctl voco :gain 1))

;;preset 1
(do 
  (ctl cs80lead-midi :note 50)
  (ctl cs80lead-midi :vibdepth 0.015)
  (ctl cs80lead-midi :vibrate 8)
  (ctl cs80lead-midi :sus 1)
  (ctl voco :voc-band-gain 10)
  (ctl voco :src-band-gain 10)
  (ctl voco :comp-gain 1.001)
  (ctl voco :fsib 8000)
  (ctl voco :sib-gain 1.0010)
  (ctl voco :rq-voc 0.15)
  (ctl voco :rq-synth 0.15)
  (ctl voco :distance 100)
  (ctl voco :gain 1))


(recording-start "~/Desktop/mic-check-09.wav")
(recording-stop)

(cs80lead-midi :note 50)
(ctl cs80lead-midi :gate 0)
(pluck)
(pm-osc)
(stop)
(square)
(pulse)
(needle-rect)
(skip-needle)
