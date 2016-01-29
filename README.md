# vocoder

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

## License

Copyright © 2016 nseymoursmith

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
