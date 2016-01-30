(ns vocoder.core
  (:use [overtone.live]))

(defn- nbpf
  "n-stage recursive band-pass filter"
  [input freq rq stages]
  (loop [n stages signal input]
    (if (zero? n) signal
        (recur (dec n) (bpf signal freq rq)))))

(defn- log-spacing
  "List of numbers, evenly spaced on a logarithmic scale, 
  'end' value inclusive"
  [start end steps]
  (let [base (Math/pow (/ end start) (/ steps))
        n (range steps)]
    (map #(* start (Math/pow base (+ % 1))) n)))

(defsynth vocoder
  "
   Vocoder emulator, designed to be attached as an fx instance to an
   instrument or to other synths via bus.

   Uses amplitude of frequency bands in an input signal (usually the mic)
   to control the amplitude of frequency bands in a synthesiser.

   Number of bands, frequency range and number of passes through the bpf
   can be adjusted when the vocoder synth is defined (but not 'live' using 
   'ctl').

   This emulator also includes a white-noise synthesizer for picking up 
   higher frequencies in speech (called sibilants).

   The thing with vocoders is that the synth settings are just as important
   as the vocoder's. You will need to experiment with your synth to get the
   the sounds you want, but these defaults work well to give a 'roboty'
   sound using the pad synth in overtone.inst.synth library (you might need
   to tweak it to gate rather than have a perc envelope though).

   Enjoy and send feedback to @nseymoursmith on twitter and github.
   "
  [bus 0 mic-input 8
   comp-thresh 0.01 comp-slope 5
   fmin 20.0 fmax 10000.0 nbands 9 stages 2 rq-voc 0.32 rq-synth 0.32
   distance 10.0
   voc-gain 1.0 src-gain 1.0 combination-gain 1.0
   fsib 8000.0 sib-gain 0.1
   gain 140.0]
  (let [; - sources - ;
        src (in bus)
        input (in mic-input)

        ; - mic compression and analysis -
        compressed (compander :in input 
                              :control input 
                              :thresh comp-thresh 
                              :slope-below comp-slope)
        voc (* voc-gain compressed)
        voc-level (amplitude:kr voc 0.1 0.1)
        band-freqs (log-spacing (. fmin value) (. fmax value) (. nbands value))
        voc-bands (map #(nbpf voc % rq-voc (. stages value)) band-freqs)
        taus (map #(/ distance %) band-freqs)
        voc-amps (map (fn [band tau] (amplitude band tau tau)) 
                      voc-bands taus)

        ; - extract and mix src bands -
        src (* src-gain src)
        src-level (amplitude:kr src 0.1 0.1)
        src-bands (map #(nbpf src % rq-synth (. stages value)) band-freqs)
        src-voc-mix (map * src-bands voc-amps)
        composited (* combination-gain (sum src-voc-mix))

        ; - white-noise synthesis for sibilance - ;
        a (buffer 2048)
        b (buffer 2048)
        noise-source (white-noise)
        formed (pv-mul (fft a compressed) (fft b noise-source))
        sib-synth (ifft formed)
        sibilance (hpf sib-synth fsib)
        sibilance (* sib-gain src-level sibilance)

        output (sum [composited sibilance])]
    (replace-out bus (* gain output))))

