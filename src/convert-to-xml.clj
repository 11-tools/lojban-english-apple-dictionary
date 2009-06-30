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
(use 'clojure.contrib.fcase)
(use 'name.choi.joshua.fnparse)

(def stop-re #"\.")

(defstruct word-s :type :rafsi :selmaho :definition :notes :keywords :etymologies)
(defstruct etymology-s :language :lojbanized :natives :transliteration :comment)
(def get-type (accessor word-s :type))
(def get-selmaho (accessor word-s :selmaho))
(def get-rafsi (accessor word-s :rafsi))
(def get-definition (accessor word-s :definition))
(def get-notes (accessor word-s :notes))
(def get-keywords (accessor word-s :keywords))
(def get-language (accessor etymology-s :language))

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

(defn parse-definitions [valsi-content]
  (-> (xml-tag-content-fn :definition) (some valsi-content) xml/content first))

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
            definition (parse-definitions content)
            notes (:content (some (xml-tag-content-fn "notes") content))
            keywords (gloss-words word)]
        [word (struct word-s word-type rafsi selmaho definition notes keywords)]))))

(defn parse-jbovlaste [source]
  (let [dict-content (:content (xml/parse source))
        e-to-l (-> dict-content parse-e-to-l map-from-pairs)
        l-to-e (parse-l-to-e dict-content e-to-l)]
    (into {} l-to-e)))

; Word origin functions

(defn language-processor [language-name source-fields transliteration-field comment-field]
  (fn [fields]
    [(fields 0)
     (struct etymology-s
       language-name (fields 2)
       (interpose "/" (filter (partial not= "") (map fields source-fields)))
       (if transliteration-field (get fields transliteration-field nil))
       (get fields comment-field nil))]))

(def language-processors
  {"src/lojban-source-words_zh.txt" (language-processor "Chinese" [3 4 5] 6 7)
   "src/lojban-source-words_es.txt" (language-processor "Spanish" [3] 4 5)
   "src/lojban-source-words_en.txt" (language-processor "English" [3] nil 4)
   "src/lojban-source-words_ru.txt" (language-processor "Russian" [3] 4 6)
   "src/lojban-source-words_hi.txt" (language-processor "Hindi" [3] 4 6)})

(defn parse-language-1 [field-processor line-seq]
  (map (comp field-processor vec (partial re-split #"\t")) line-seq))

(defn parse-languages []
  (map-from-pairs (mapcat #(parse-language-1 (val %) (read-lines (key %)))
                          language-processors)))

; XML escape character functions.

(def xml-escaped-chars
  {#"<" "&lt;"
   #">" "&gt;"
   #"&" "&amp;"
   #"'" "&apos;"
   #"\"" "&quot;"})

(def id-escaped-chars
  {#"'" "h"
   #"," "-"
   #"\." "_"
   #"\s" "--"})

(def replace-semicolons (partial re-gsub #";" "[SEMICOLON]"))

(defn replace-escape-chars [escaped-chars string]
  (if string
    (loop [cur-string string, cur-escaped-char-seq escaped-chars]
      (if-not (empty? cur-escaped-char-seq)
        (let [[pattern replacement] (first cur-escaped-char-seq)]
          (recur (s/replace cur-string pattern replacement)
                 (rest cur-escaped-char-seq)))
        cur-string))))

(def replace-xml-escape-chars
  (comp (partial replace-escape-chars xml-escaped-chars) replace-semicolons))

(def replace-id-escape-chars (partial replace-escape-chars id-escaped-chars))

; Dump data as Apple dictionary XML.

(defn transform-string [string process]
  (if (empty? string) "" (process string)))

(def split-definitions (partial re-split #"\s*\[SEMICOLON\]\s*"))

(def replace-definition-vars
  (partial re-gsub #"\$x\{(\d+)\}\$"
    #(let [variable (get % 1)]
       (str "<var>x" variable "</var>"))))

(defn split-rafsi [x]
  (if (= x "") nil (re-split #"\s+" x)))

(def transform-definitions
  (partial map (partial format "<li>%s</li>")))

(def join-definitions
  (partial s/join "\n"))

(defn- prepare-definition [string]
  (-> string
    replace-definition-vars
    split-definitions
    transform-definitions
    join-definitions))

(def remove-bad-indexes
  (partial remove #(or (nil? %) (= "the" %) (= "" %))))

(def transform-indexes
  (partial map (partial format "<d:index d:value=\"%s\"/>")))

(def split-notes
  (comp (partial re-gsub #"\[SEMICOLON\]" ";") str))

(defn- prepare-indexes [word keywords rafsi]
  (let [stripped-word (re-gsub stop-re "" word)]
    (-> #{word stripped-word} (into rafsi) (into keywords) remove-bad-indexes
        transform-indexes join-definitions)))
;(defn- prepare-indexes [word keyword rafsi]
;  (let [stripped-word (re-gsub stop-re "" word)
;        keyword-tokens (re-split #"\s+" keyword)]
;    (-> #{word stripped-word keyword} (into keyword-tokens) (into rafsi) remove-bad-indexes
;        transform-indexes join-definitions)))

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

(defn- prepare-notes [string]
  (-> string split-notes
    (transform-string (partial format "<p class=\"note\">%s</p>"))))

(defn- make-etymology-table [etymology-data word]
  (let [etymologies (etymology-data word)]
    (if (empty? etymologies)
      ""
      (str-flatten
        ["<h2>Etymologies</h2>\n<table>\n<tr><th>Language</th><th>Lojbanized</th><th>Native</th><th>Translation</th><th>Comment</th></tr>\n"
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
<p>Based on the Jbovlaste dictionary and the word lists from the Logical Language Group of the 1990s.</p>
<p>Last updated on 2009-06-26.</p>
<ul>

<li><a href=\"http://www.lojban.org/publications/reference_grammar/chapter1\">The Complete Lojban Language</a></li>

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
          notes (get-notes word-datum)
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
</d:entry>
"
        word-id word (prepare-indexes word keywords rafsi) word type
        (prepare-secondary-info word-datum word rafsi type) (prepare-definition definition)
        (prepare-notes notes) etymology-table)))
  (println "</d:dictionary>"))

(defn main- []
  (let [; This is where the word data is read from the Jbovlaste XML dump.
        word-data (parse-jbovlaste "src/xml-export.html")
        ; This is where the word origin data is read.
        etymology-data (parse-languages)]
;    (println word-data)))
    (dump-xml word-data etymology-data)))

(main-)













