
###
###
### Bootstrap
###
###

(do

                                        # Deprecate file-popen
 (when-let [v (get root-env 'file-popen)]
   (put v :deprecated true))

                                        # Modify root-env to remove private symbols and
                                        # flatten nested tables.
 (loop [[k v] :in (pairs root-env)
        :when (symbol? k)]
       (def flat (tab-proto-flatten v))
       (when (boot/config :no-docstrings)
         (put flat :doc nil))
       (when (boot/config :no-sourcemaps)
         (put flat :source-map nil))
                                        # Fix directory separators on windows to make image identical between windows and non-windows
       (when-let [sm (get flat :source-map)]
         (put flat :source-map [(s/>* "\\" "/" (sm 0)) (sm 1) (sm 2)]))
       (if (v :private)
           (put root-env k nil)
         (put root-env k flat)))
 (put root-env 'boot/config nil)
 (put root-env 'boot/args nil)

                                        # Build dictionary for loading images
 (def load-dict (env-lookup root-env))
 (each [k v] (pairs load-dict)
       (if (number? v) (put load-dict k nil)))
 (merge-into load-image-dict load-dict)

 (def image
      (let [env-pairs (pairs (env-lookup root-env))
                      essential-pairs (filter (fn [[k v]] (or (cfunction? v) (abstract? v))) env-pairs)
                      lookup (table ;(mapcat identity essential-pairs))
                      reverse-lookup (invert lookup)]
                                        # Check no duplicate values
        (def temp @{})
        (eachp [k v] lookup
               (if (in temp v) (errorf "duplicate value: %v" v))
               (put temp v k))
        (marshal root-env reverse-lookup)))

                                        # Create amalgamation

 (def feature-header "src/core/features.h")

 (def local-headers
      ["src/core/state.h"
       "src/core/util.h"
       "src/core/gc.h"
       "src/core/vector.h"
       "src/core/fiber.h"
       "src/core/regalloc.h"
       "src/core/compile.h"
       "src/core/emit.h"
       "src/core/symcache.h"])

 (def core-sources
      ["src/core/abstract.c"
       "src/core/array.c"
       "src/core/asm.c"
       "src/core/buffer.c"
       "src/core/bytecode.c"
       "src/core/capi.c"
       "src/core/cfuns.c"
       "src/core/compile.c"
       "src/core/corelib.c"
       "src/core/debug.c"
       "src/core/emit.c"
       "src/core/ev.c"
       "src/core/fiber.c"
       "src/core/gc.c"
       "src/core/inttypes.c"
       "src/core/io.c"
       "src/core/marsh.c"
       "src/core/math.c"
       "src/core/net.c"
       "src/core/os.c"
       "src/core/parse.c"
       "src/core/peg.c"
       "src/core/pp.c"
       "src/core/regalloc.c"
       "src/core/run.c"
       "src/core/specials.c"
       "src/core/state.c"
       "src/core/string.c"
       "src/core/strtod.c"
       "src/core/struct.c"
       "src/core/symcache.c"
       "src/core/table.c"
       "src/core/tuple.c"
       "src/core/util.c"
       "src/core/value.c"
       "src/core/vector.c"
       "src/core/vm.c"
       "src/core/wrap.c"])

                                        # Print janet.c to stdout
 (print "/* Amalgamated build - DO NOT EDIT */")
 (print "/* Generated from janet version " janet/version "-" janet/build " */")
 (print "#define JANET_BUILD \"" janet/build "\"")
 (print ```#define JANET_AMALG```)

 (defun do-one-file
     [fname]
   (print "\n/* " fname " */")
   (print "#line 0 \"" fname "\"\n")
   (def source (slurp fname))
   (print (s/>* "\r" "" source)))

 (do-one-file feature-header)

 (print ```#include "janet.h"```)

 (each h local-headers
       (do-one-file h))

                                        # windows.h should not be included in any of the external or internal headers - only in .c files.
 (print)
 (print "/* Windows work around - winsock2 must be included before windows.h, especially in amalgamated build */")
 (print "#if defined(JANET_WINDOWS) && defined(JANET_NET)")
 (print "#include <winsock2.h>")
 (print "#endif")
 (print)

 (each s core-sources
       (do-one-file s))

                                        # Create C source file that contains images a uint8_t buffer. This
                                        # can be compiled and linked statically into the main janet library
                                        # and example client.
 (print "static const unsigned char janet_core_image_bytes[] = {")
 (loop [line :in (partition 16 image)]
       (prin "  ")
       (each b line
             (prinf "0x%.2X, " b))
       (print))
 (print "  0\n};\n")
 (print "const unsigned char *janet_core_image = janet_core_image_bytes;")
 (print "size_t janet_core_image_size = sizeof(janet_core_image_bytes);"))
