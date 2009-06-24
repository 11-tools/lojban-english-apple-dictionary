; This script was last used with Clojure 1.0

; java -cp /Users/joshuachoi/Development/clojure/clojure-1.0.0.jar:/Users/joshuachoi/Development/clojure-contrib/clojure-contrib.jar:../FnParse/src/:./src/:./test/ clojure.main src/convert-to-xml.clj | tee src/Lojban-English.xml
; cd src; make; make install; cd ..

(use 'clojure.contrib.duck-streams)
(use 'clojure.contrib.str-utils)
(use 'clojure.contrib.seq-utils)
(use 'clojure.contrib.fcase)

(def stop-re #"\.")

(defstruct word-s :type :word :rafsi :selmaho :keyword :hint :definition :textbook
                  :frequency :misc-info :etymologies)
(defstruct etymology-s :language :lojbanized :natives :transliteration :comment)
(def get-type (accessor word-s :type))
(def get-word (accessor word-s :word))
(def get-keyword (accessor word-s :keyword))
(def get-selmaho (accessor word-s :selmaho))
(def get-rafsi (accessor word-s :rafsi))
(def get-definition (accessor word-s :definition))
(def get-frequency (accessor word-s :frequency))
(def get-misc-info (accessor word-s :misc-info))
(def get-language (accessor etymology-s :language))

(def apply-str (partial apply str))
(def str-flatten (comp apply-str flatten))

(defn map-from-pairs [pairs]
  (reduce (fn [a-map [k v]] (assoc a-map k (conj (get a-map k []) v))) {} pairs))

; Word data functions

(def xml-escaped-chars
  {#"<" "&lt;"
   #">" "&gt;"
   #"&" "&amp;"
   #"'" "&apos;"
   #"\"" "&quot;"})

(def id-escaped-chars
  {#"&apos;" "h"
   #"\." "_"
   #"\s" "-"})

(def sub-semicolons (partial re-gsub #";" "[SEMICOLON]"))

(defn sub-escape-chars [escaped-chars string]
  (loop [cur-string string, cur-escaped-char-seq escaped-chars]
    (if-not (empty? cur-escaped-char-seq)
      (let [[pattern replacement] (first cur-escaped-char-seq)]
        (recur (re-gsub pattern replacement cur-string)
               (rest cur-escaped-char-seq)))
      cur-string)))

(def sub-xml-escape-chars (comp (partial sub-escape-chars xml-escaped-chars) sub-semicolons))
(def sub-id-escape-chars (partial sub-escape-chars id-escaped-chars))

(def gismu-columns [[:word 0 6] [:rafsi 7 19] [:keyword 20 39] [:hint 41 60]
                    [:definition 62 157] [:textbook 160 162] [:frequency 163 164]
                    [:misc-info 165 nil]])

(def cmavo-columns [[:word 0 10] [:selmaho 11 19] [:keyword 20 61] [:definition 62 167]
                    [:misc-info 168 nil]])

(defn parse-data [[line-seq column-limits word-type]]
  (into {}
    (map #(vector (sub-id-escape-chars (get-word %)) %)
      (for [line line-seq]
        (let [line-length (count line)]
          (apply struct-map word-s
            (concat [:type word-type]
              (apply concat
                (for [[key l-column r-column] column-limits :when (< l-column line-length)]
                  [key
                   (sub-xml-escape-chars (.trim (if (and r-column (< r-column line-length))
                                                  (subs line l-column r-column)
                                                  (subs line l-column))))])))))))))

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

; This is where the word data is read from the TXT files.

(def gismu-lines (rest (read-lines "src/gismu.txt")))
(def cmavo-lines (rest (read-lines "src/cmavo.txt")))

(def word-data
  (apply merge (map parse-data [[gismu-lines gismu-columns "gismu"]
                                [cmavo-lines cmavo-columns "cmavo"]])))

; This is where the word origin data is read.

(def etymology-data (parse-languages))

;(map (partial println ">>>") etymology-data)

; Dump word data as Apple dictionary XML.

(defn transform-string [string process]
  (if (empty? string) "" (process string)))

(def split-definitions (partial re-split #"\s*\[SEMICOLON\]\s*"))
;(def split-definitions (partial re-split #"\s*;\s*"))
(def sub-definition-vars (partial re-gsub #"(x\d)" (fn [[_ variable]]
                                                      (str "<var>" variable "</var>"))))
(defn split-rafsi [x]
  (if (= x "") nil (re-split #"\s+" x)))
(def transform-definitions (partial map (partial format "<li>%s</li>")))
(def join-definitions (partial str-join "\n"))
(def remove-bad-indexes (partial remove #(or (nil? %) (= "the" %) (= "" %))))
(def transform-indexes (partial map (partial format "<d:index d:value=\"%s\"/>")))
(def split-misc-info (comp (partial re-gsub #"\[SEMICOLON\]" ";") str))

(defn- prepare-indexes [word keyword rafsi]
  (let [stripped-word (re-gsub stop-re "" word)
        keyword-tokens (re-split #"\s+" keyword)]
    (-> #{word stripped-word keyword} (into keyword-tokens) (into rafsi) remove-bad-indexes
        transform-indexes join-definitions)))

(defn- prepare-secondary-info [word-datum word rafsi word-type]
  (let [secondary-info
        (case word-type
          "gismu"
            (cons "rafsi: "
              (interpose ", " (map #(vector "<strong>" % "</strong>") (cons word rafsi))))
          "cmavo"
            (cons "selma'o: " (split-definitions (get-selmaho word-datum))))]
    (str-flatten [" ( " secondary-info " )"])))

(defn- prepare-definition [string]
  (-> string sub-definition-vars split-definitions transform-definitions join-definitions))

(defn- prepare-misc-info [string]
  (-> string split-misc-info
    (transform-string (partial format "<p class=\"note\">%s</p>"))))

(defn- make-etymology-table [word]
  (let [etymologies (etymology-data word)]
    (if (empty? etymologies)
      ""
      (str-flatten
        ["<h2>Etymologies</h2>\n<table>\n<tr><th>Source language</th><th>Lojbanized word</th><th>Native word</th><th>Word translation</th><th>Comment</th></tr>\n"
         (map (fn [etymology] (vector "<tr>" (map #(vector "<td>" (val %) "</td>") etymology) "</tr>\n"))
           etymologies)
     "</table>\n"]))))

(defn dump-xml [data]
  (println "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  (println "<d:dictionary xmlns=\"http://www.w3.org/1999/xhtml\"")
  (println "  xmlns:d=\"http://www.apple.com/DTDs/DictionaryService-1.0.rng\">\n")
  
  (println "<d:entry id=\"front-back-matter\" d:title=\"(front/back matter)\">")
  (println "<h1>The Lojban Dictionary in English</h1>")
  (println "<p>Based on the word lists from the Logical Language Group of the 1990s</p>")
  (println "</d:entry>")
  
  (doseq [[word-id word-datum] data]
    (let [type (get-type word-datum)
          word (get-word word-datum)
          keyword (get-keyword word-datum)
          rafsi (if (= type "gismu") (split-rafsi (get-rafsi word-datum)))
          definition (get-definition word-datum)
          frequency (get-frequency word-datum)
          misc-info (get-misc-info word-datum)
          etymology-table (if (= type "gismu") (make-etymology-table word) "")]
      (printf "<d:entry id=\"%s\" d:title=\"%s\">

%s
<h1>%s</h1>
<p class=\"word-type\">%s%s</p>
<ul>
%s
</ul>
%s<p class=\"minor-note\">Frequency: %s</p>
%s
</d:entry>
"
        word-id word (prepare-indexes word keyword rafsi) word type
        (prepare-secondary-info word-datum word rafsi type) (prepare-definition definition)
        (prepare-misc-info misc-info) (or frequency "undefined") etymology-table)))
  (println "</d:dictionary>"))

(dump-xml word-data)














