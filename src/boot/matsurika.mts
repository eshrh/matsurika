
###
###
### MATSURIKA
###
###

## String ops

(defmacro s-in?
  [patt str]
  ~(truthy? (s> ,patt ,str)))

(defmacro s:>
  "Slice STR from beginning until FIND"
  [str find]
  ~(s: ,str 0 (s> ,find ,str)))

(defmacro s>:
  "Slice STR from FIND until end"
  [str find]
  ~(s: ,str (s> ,find ,str)))

(defmacro s>:>
  "Slice STR from FBEG until FEND"
  [str fbeg fend]
  ~(s: ,str (s> ,fend ,str) (s> ,fend ,str)))

(defmacro lines
  "Split string by \n"
  [str]
  ~(s/ "\n" ,str))

(defun words
  "Split string by any amount of spaces"
  [str]
  (filter |(not (empty? $))
          (map s// (s/ " " str))))

(defmacro letters
  [str]
  ~(map s-from-bytes ,str))

(defmacro s+
  "Concat strings with extra vars.
Variables:
qt - double quote
sqt - single quote
nl - newline
tb - tab
s - space"
  [& strings]
  ~(let [qt "\""
         sqt "'"
         nl "\n"
         tb "\t"
         s " "]
     (string ,;strings)))

## Other, functional things
(defun id (x) x)

(defun const [f]
  (fn [x] (f)))

(defun is-len?
  "Checks if COLL is LEN"
  [coll len]
  (= (length coll) len))

(defun head
  "Gets all but the last element of XS"
  [xs]
  (take (- (length xs) 1) xs))

(defun tail
  "Gets all but the first element of XS"
  [xs]
  (drop 1 xs))

(defun fst
  "Gets the first element of DS"
  [ds]
  (first ds))

(defun snd
  "Gets the second element of DS"
  [ds]
  (get ds 1))

(defun thread-flip [form]
  ~((flip ,(fst form)) ,;(tail form)))

(defmacro ->>
  [x & forms]
  (var formsmut @[])
  (var i 0)
  (while (< i (length forms))
    (let [cur (get forms i)]
      (if (= '* cur)
        (do
          (arr<- formsmut
                 (thread-flip (get forms (inc i))))
          (+= i 2))
        (do
          (arr<- formsmut cur)
          (+= i 1)))))
  ~(thread-last ,x ,;formsmut))

## File reading functions

(defun file<-
  "Read all the data of a file, return a string"
  [file-path]
  (let [f (file-open file-path :rn)]
    (var content (file-read f :all))
    (file-close f)
    content))

(defun file->
  "Dump to file-path a string content"
  [file-path content]
  (let [f (file-open file-path :wn)]
    (file-write f content)
    (file-close f)))

(defun file+
  "Append to file-path a string content"
  [file-path content]
  (let [f (file-open file-path :a+n)]
    (file-write f)
    (file-close f)))

(defmacro file-each-line
  "Execute forms on each line of file. variable
line contains line as string."
  [file & forms]
  ~(do
    (var line (file-read ,file :line))
    (while line
      ,;forms
      (set line (file-read ,file :line)))))

(defun list-replace
  "Replace all elements of a tuple. PRED is a predicate that if t
decides to replace the element. TO-FUNC is a function that returns what to replace
the element with. Both functions take a single argument, the element."
  [pred to-func form]
  (tuple ;(map (fn [el]
                 (cond (pred el) (to-func el)
                       (tuple? el) (list-replace pred to-func el)
                       el))
               form)))

(defmacro awk-dispatch [code rawline]
  (def code (list-replace (fn [el] (s-prefix? "_" (string el)))
                          (fn [el] (tuple 'f (s->n (tail (string el)))))
                          (if (tuple? code)
                            code ~(id ,code))))
  ~(let [line (s// ,rawline)
         wds (words line)
         wdsn (map s->n wds)
         f (fn [i] (get wds i))
         n (fn [i] (s->n (f i)))
         NF (length wds)]
     ,code))

(defun awk-str [data str]
  (var res @[])
  (each line (lines str)
        (each action (pairs data)
              (let [[patt code] action]
                (if (peg>! patt line)
                  (arr<- res (eval ~(awk-dispatch ,code ,line)))))))
  res)

(defun awk-file
  "Execute actions on every line of a file (object).
First arg is a table, key is a peg and value is a single form to be run on every
line that the peg matches. Forms have access to several variables:
line: trimmed line
wds: line split by whitespace
wdsn: words of the line, all converted to numbers
f: a function to get the nth field of the line
n: a function to get the nth field of the line, as a number
NF: number of fields
_<number>: shorthand for (f <number>)"
  [data file]
  (var res @[])
  (def f (if (string? file) (file-open file) file))
  (file-each-line
   f
   (each action (pairs data)
         (let [[patt code] action]
           (if (peg>! patt line)
             (arr<- res (eval ~(awk-dispatch ,code ,line)))))))
  res)

(defmacro awk [data]
  ~(pp (awk-file ',data stdin)))

## PEG helper

(defmacro peg>!*
  "Get all matching peg text matching PATT in TEXT"
  [patt text]
  ~(map |(peg>! ,patt ,text $) (peg>* ,patt ,text)))

## Flipped indexing

(defun flip
  "Return a new function with the args flipped"
  [f]
  (fn [& args]
      (apply f (reverse args))))

(defun idx
  "Index k'th item of ds"
  [k ds]
  (if (pos? k)
      (get ds k)
    (get ds (+ (length ds) k))))

## Shell commands
(defun- sh--get-cmd [fst args]
  (map id (list-replace (fn [el] (s-prefix? "$" el))
                        (fn [el] (->> (s: el 1) (symbol) (eval) (string)))
                        (if (tuple? fst)
                          (map string fst)
                          (map string args)))))

(defmacro sh-run
  "Run as shell command. Prints output, returns stat"
  [& args]
  (def fst (get args 0))
  ~(os-shell ,(s-join (sh--get-cmd fst args) " ")))

(defmacro sh-run-do
  "Run multiple forms as shell commands"
  [& forms]
  ~(do ,;(map sh-run forms)))

(defmacro sh-run-cmd
  "Execute program. Returns output string."
  [& args]
  (let [fst (get args 0)
        cmd (sh--get-cmd fst args)]
    ~(lines (do
             (def p
                  (os-spawn ,cmd :p {:in :pipe :out :pipe}))
             (:wait p)
             (:read (p :out) :all)))))

(defmacro sh-run-cmd-do
  "Execute multiple programs"
  [& forms]
  (def forms (map sh-run-cmd forms))
  ~(map eval ,forms))

(defmacro $
  "Entry point to shell macro. Run as shell command if last arg is :"
  [& args]
  (if (= (last args) :sh)
    ~(sh-run ,(head args))
    ~(sh-run-cmd ,args)))

(defmacro $*
  "Run many shell macros."
  [& forms]
  (let [lastform (last forms)]
    (if (and
          (not (tuple? lastform))
          (= lastform :sh))
      ~(sh-run-do ,;(head forms))
      ~(sh-run-cmd-do ,;forms))))

## cli stuff

(defmacro cli
  [& forms]
  ~(defun main [& args]
     ,;forms))