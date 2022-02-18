# Patch janet.h 
(def [_ janeth janetconf output] (dyn :args))
(spit output (s/> `#include "janetconf.h"` (slurp janetconf) (slurp janeth)))
