; This script was last used with Clojure 1.0

; Meant to be run from inside the project folder, just outside the src folder.
; java -cp /Users/joshuachoi/Development/clojure/clojure-1.0.0.jar:/Users/joshuachoi/Development/clojure-contrib/clojure-contrib.jar:../FnParse/src/:./src/:./test/ clojure.main src/convert-to-xml.clj | tee src/Lojban-English.xml
; java -jar ~/Development/jing/bin/jing.jar /Developer/Extras/Dictionary\ Development\ Kit/documents/DictionarySchema/AppleDictionarySchema.rng src/Lojban-English.xml
; cd src; make; make install; make clean; cd ..

(require '[clojure.xml :as xml])
(require '[clojure.contrib.str-utils2 :as s])
(use 'clojure.contrib.duck-streams)
(use 'clojure.contrib.str-utils)
(use 'clojure.contrib.seq-utils)
(use 'clojure.contrib.except)
(use 'clojure.contrib.fcase)
(use 'name.choi.joshua.fnparse)

;(set! *warn-on-reflection* true)

(def stop-re #"\.")

(defstruct word-s :type :rafsi :selmaho :definition :notes :keywords :etymologies)
(defstruct etymology-s :language :lojbanized :natives :transliteration :translation)
(def get-type (accessor word-s :type))
(def get-selmaho (accessor word-s :selmaho))
(def get-rafsi (accessor word-s :rafsi))
(def get-definition (accessor word-s :definition))
(def get-notes (accessor word-s :notes))
(def get-keywords (accessor word-s :keywords))
(def get-language (accessor etymology-s :language))

(defn last-char [string]
  (.charAt string (-> string count dec)))

(def apply-str (partial apply str))

(def str-flatten (comp apply-str flatten))

(defn map-from-pairs [pairs]
  (reduce (fn [a-map [k v]] (assoc a-map k (conj (get a-map k []) v))) {} pairs))

; Word data functions

(defn certain-direction-node [from-lang to-lang direction-nodes]
  (some
    (fn [each-node]
      (let [node-attrs (xml/attrs each-node)]
        (if (and (= (:from node-attrs) from-lang) (= (:to node-attrs) to-lang))
          each-node)))
    direction-nodes))

(defn xml-tag-content-fn [xml-tag]
  (fn [xml-node]
    (if (= (xml/tag xml-node) xml-tag)
      xml-node)))

(defn parse-vector-content [node-tag valsi-content]
  (map (comp first xml/content) (filter (xml-tag-content-fn node-tag) valsi-content)))

(defn parse-single-content [node-tag valsi-content]
  (let [node (-> node-tag xml-tag-content-fn (some valsi-content))]
    (if node
      (-> node xml/content first))))

(defn parse-e-to-l [dict-content]
  (let [dict-direction (certain-direction-node "English" "lojban" dict-content)]
    (for [nlword (xml/content dict-direction)]
      (let [attrs (xml/attrs nlword)
            word (:word attrs)
            valsi (:valsi attrs)
            sense (:sense attrs)]
        [valsi (apply str word (if sense [" (" sense ")"]))]))))

(defn parse-l-to-e [dict-content gloss-words]
  (let [dict-direction (certain-direction-node "lojban" "English" dict-content)]
    (for [valsi (xml/content dict-direction)]
      (let [attrs (xml/attrs valsi)
            word-type (:type attrs)
            word (:word attrs)
            content (xml/content valsi)
            rafsi (parse-vector-content :rafsi content)
            selmaho (parse-vector-content :selmaho content)
            definition (parse-single-content :definition content)
            notes (parse-single-content :notes content)
            keywords (gloss-words word)]
        [word (struct word-s word-type rafsi selmaho definition notes keywords)]))))

(defn parse-jbovlaste [source]
  (let [dict-content (:content (xml/parse source))
        e-to-l (-> dict-content parse-e-to-l map-from-pairs)
        l-to-e (parse-l-to-e dict-content e-to-l)]
    (into {} l-to-e)))

; Word origin functions

(defn language-processor
  [language-name source-fields transliteration-field translation-field]
  (fn [fields]
    [(fields 0)
     (struct etymology-s
       language-name (fields 2)
       (interpose "/" (filter (partial not= "") (map fields source-fields)))
       (if transliteration-field (get fields transliteration-field "???") "~")
       (if translation-field (get fields translation-field "???") "~"))]))

(def language-processors
  {"src/lojban-source-words_zh.txt" (language-processor "Chinese" [3 4] 5 6)
   "src/lojban-source-words_es.txt" (language-processor "Spanish" [3] nil 4)
   "src/lojban-source-words_en.txt" (language-processor "English" [3] nil nil)
   "src/lojban-source-words_ru.txt" (language-processor "Russian" [3] 4 5)
   "src/lojban-source-words_hi.txt" (language-processor "Hindi" [3] 4 5)})

(defn parse-language-1 [field-processor line-seq]
  (map (comp field-processor vec (partial re-split #"\t")) line-seq))

(defn parse-languages []
  (map-from-pairs (mapcat #(parse-language-1 (val %) (read-lines (key %)))
                          language-processors)))

; XML escape character functions.

(def xml-escaped-chars
  [[#";" "`SEMICOLON`"]
   [#"<" "&lt;"]
   [#">" "&gt;"]
   [#"&" "&amp;"]
   [#"'" "&apos;"]
   [#"\"" "&quot;"]])

(def id-escaped-chars
  {#"'" "h"
   #"," "-"
   #"\." "_"
   #"\s" "--"})

(defn replace-escape-chars [escaped-chars string]
  (if string
    (loop [cur-string string, cur-escaped-char-seq escaped-chars]
      (if (seq cur-escaped-char-seq)
        (let [esc-pair (first cur-escaped-char-seq)
              pattern (esc-pair 0)
              replacement (esc-pair 1)]
          (recur (s/replace cur-string pattern replacement)
                 (rest cur-escaped-char-seq)))
        cur-string))))

(def replace-xml-escape-chars
  (partial replace-escape-chars xml-escaped-chars))

(def replace-id-escape-chars
  (partial replace-escape-chars id-escaped-chars))

; Splitting definitions and notes on commas outside parentheses and brackets

(def line-separator-r
  (lit-conc-seq "`SEMICOLON`"))

(def replaced-semicolon-r
  (constant-semantics line-separator-r ";"))

(def regular-text-r
  (rep+ (except anything line-separator-r)))

(defn delimited-text-rm [prefix suffix]
  (complex [content-prefix (lit prefix)
            content (rep* (except (alt replaced-semicolon-r anything) (lit suffix)))
            content-suffix (lit suffix)]
    [content-prefix content content-suffix]))

(def parentheses-text-r
  (delimited-text-rm \( \)))

(def bracketed-text-r
  (delimited-text-rm \[ \]))

(def delimited-text-r
  (alt parentheses-text-r bracketed-text-r))

(def line-r
  (rep* (alt delimited-text-r regular-text-r)))

(def ws-r
  (rep* (lit \space)))

(def separated-line-r
  (complex [_ ws-r, _ (lit-conc-seq "`SEMICOLON`"), _ ws-r, line line-r]
    line))

(def definition-r
  (complex [first-line line-r, rest-lines (rep* separated-line-r)]
    (map str-flatten (cons first-line rest-lines))))

(defn split-list-items [string]
  (binding [*remainder-accessor* identity
            *remainder-setter* #(identity %2)]
    (rule-match definition-r
      #(throwf "ERROR invalid definition %s" %)
      #(throwf "ERROR leftovers in definition \"%s\" \"%s\"" %1 (apply-str %2))
      string)))

; Dump data as Apple dictionary XML.

(defn transform-string [string process]
  (if (empty? string) "" (process string)))

(defn match-latex [string]
  ; Also! Math powers may take the form 8^{23}.
  (if-let [power-match (re-matches #"(\d+)\^(?:\{(\-?\d+)\}|(\d+))" string)]
    (str (power-match 1) "<sup>" (or (power-match 2) (power-match 3)) "</sup>")
    (format "<var>%s</var>"
      (str-flatten
        (map
          #(if-let [match (re-matches #"(\w)+_(?:\{(\d+)\}|(\d+))" %)]
             (let [var-name (match 1)
                   var-num (or (match 2) (match 3))]
               [var-name "<sub>" var-num "</sub>"])
             (throwf "ERROR invalid LaTEX expression: %s" string))
          (re-split #"\s*=\s*" string))))))

(def replace-indicators
  (comp (partial re-gsub #"\{|\}" "")
        (partial re-gsub #"\$([^$]+)\$" #(match-latex (% 1)))))

(def replace-semicolons
  (partial re-gsub #"`SEMICOLON`" ";"))

(defn split-rafsi [x]
  (if (= x "") nil (re-split #"\s+" x)))

(def transform-list-items
  (partial map
    #(if (= (last-char %) \.)
       (format "<li>%s</li>" %)
       (format "<li>%s.</li>" %))))

(def join-list-items
  (partial str-join "\n"))

(defn- prepare-definition [string]
  (-> string
    replace-indicators
    split-list-items
    transform-list-items
    join-list-items))

(def remove-bad-indexes
  (partial remove #(or (nil? %) (= "the" %) (= "" %))))

(def transform-indexes
  (partial map (comp (partial format "<d:index d:value=\"%s\"/>")
                     replace-semicolons)))

(defn- prepare-indexes [word keywords rafsi]
  (let [stripped-word (re-gsub stop-re "" word)]
    (-> #{word stripped-word} (into rafsi) (into keywords) remove-bad-indexes
        transform-indexes join-list-items)))
;(defn- prepare-indexes [word keyword rafsi]
;  (let [stripped-word (re-gsub stop-re "" word)
;        keyword-tokens (re-split #"\s+" keyword)]
;    (-> #{word stripped-word keyword} (into keyword-tokens) (into rafsi) remove-bad-indexes
;        transform-indexes join-list-items)))

(defn- prepare-secondary-info [word-datum word rafsi word-type]
  (if-let [secondary-info
           (case word-type
             "gismu"
               (cons "rafsi: "
                 (interpose ", " (map #(vector "<strong>" % "</strong>")
                                   (cons word rafsi))))
             "cmavo"
               (cons "selma'o: " (get-selmaho word-datum)))]
    (str-flatten [" ( " secondary-info " )"])
    ""))

(def transform-notes
  (partial format "<p class=\"notes\">%s</p>"))

(defn- prepare-notes [string]
  (if (or (nil? string) (= string ""))
    ""
    (-> string replace-indicators replace-semicolons 
      transform-notes)))

(defn- make-etymology-table [etymology-data word]
  (let [etymologies (etymology-data word)]
    (if (empty? etymologies)
      ""
      (str-flatten
        ["<h2>Etymologies</h2>\n<table>\n<tr><th>Language</th><th>Lojbanized</th><th>Native</th><th>Transliteration</th><th>Translation</th></tr>\n"
         (map
          (fn [etymology]
            (vector "<tr>" (map #(vector "<td>" (val %) "</td>") etymology) "</tr>\n"))
           etymologies)
     "</table>\n"]))))

(defn dump-xml [data etymology-data]
  (println "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<d:dictionary xmlns=\"http://www.w3.org/1999/xhtml\"
  xmlns:d=\"http://www.apple.com/DTDs/DictionaryService-1.0.rng\">

<d:entry id=\"front-back-matter\" d:title=\"(front/back matter)\">

<div class=\"matter\">

<h1>The Lojban Dictionary in English</h1>
<p>Entries are taken on the <a href=\"http://www.lojban.org/publications/reference_grammar/chapter1\">Jbovlaste dictionary</a>. Gismu etymologies are thanks to <a href=\"https://www.dealloc.org/~mublin/\">Mublin</a>, who worked hard to compile them.</p>
<p>Last updated on 2009-07-01.</p>
<ul>

<li><a href=\"http://www.lojban.org\">The official Lojban Website</a></li>
<li><a href=\"http://www.lojban.org/publications/reference_grammar/chapter1\">The Complete Lojban Language</a></li>
<li><a href=\"http://www.lojban.org/publications/reference_grammar/chapter1\">The Complete Lojban Language</a></li>
<li><a href=\"http://www.lojban.org/publications/reference_grammar/chapter1\">Jbovlaste, the de facto editable community dictionary</a></li>
<li><a href=\"https://www.dealloc.org/~mublin/\">Mublin's Lojban etymologies</a></li>

</ul>

</div>
</d:entry>")
  
  (doseq [data-pair data]
    (let [word (data-pair 0)
          word-datum (data-pair 1)
          type (get-type word-datum)
          word-id (replace-id-escape-chars word)
          keywords (map replace-xml-escape-chars (get-keywords word-datum))
          rafsi (if (= type "gismu") (get-rafsi word-datum))
          definition (-> word-datum get-definition replace-xml-escape-chars)
          notes (-> word-datum get-notes replace-xml-escape-chars)
          etymology-table (if (= type "gismu")
                            (make-etymology-table etymology-data word)
                            "")]
      (printf "<d:entry id=\"%s\" d:title=\"%s\">
%s
<h1>%s</h1>
<p class=\"word-type\">%s%s</p>
<ul>
%s
</ul>
%s
%s
</d:entry>"
        word-id word (prepare-indexes word keywords rafsi) word type
        (prepare-secondary-info word-datum word rafsi type) (prepare-definition definition)
        (prepare-notes notes) etymology-table)))
  (println "\n</d:dictionary>"))

(defn main- []
  (let [; This is where the word data is read from the Jbovlaste XML dump.
        word-data (parse-jbovlaste "src/xml-export.xml")
        ; This is where the word origin data is read.
        etymology-data (parse-languages)]
;    (println word-data)))
    (dump-xml word-data etymology-data)))

(main-)













