; This script was last used with Clojure 1.0

(use 'clojure.contrib.duck-streams)
(use 'clojure.contrib.str-utils)

(def stop-re #"\.")

(def gismu-lines (rest (read-lines "gismu.txt")))
(def cmavo-lines (rest (read-lines "cmavo.txt")))

(defstruct word-s :type :word :rafsi :keyword :hint :definition :textbook :frequency
                  :misc-info)
(def get-type (accessor word-s :type))
(def get-word (accessor word-s :word))
(def get-keyword (accessor word-s :keyword))
(def get-definition (accessor word-s :definition))
(def get-frequency (accessor word-s :frequency))
(def get-misc-info (accessor word-s :misc-info))

(def xml-escaped-chars
  {#"<" "&lt;"
   #">" "&gt;"
   #"&" "&amp;"
   #"'" "&apos;"
   #"\"" "&quot;"})

(defn sub-xml-escape-chars [string]
  (loop [cur-string string, cur-escaped-char-seq xml-escaped-chars]
    (if-not (empty? cur-escaped-char-seq)
      (let [[pattern replacement] (first cur-escaped-char-seq)]
        (recur (re-gsub pattern replacement cur-string)
               (rest cur-escaped-char-seq)))
      cur-string)))

(defn sub-id-escape-chars [string]
  (re-gsub #"&apos;" "-" string))

(def gismu-columns [[:word 0 6] [:rafsi 7 19] [:keyword 20 39] [:hint 41 60]
              [:definition 62 157] [:textbook 160 162] [:frequency 163 169]
              [:misc-info 170 nil]])

(def cmavo-columns [[:word 0 11] [:keyword 20 39] [:hint 41 60]
                    [:definition 62 157] [:textbook 160 162] [:frequency 163 168]
                    [:misc-info 169 nil]])

(defn parse-data [[line-seq column-limits word-type]]
  (for [line line-seq]
    (let [line-length (count line)]
      (apply struct-map word-s
        (concat [:type word-type]
          (apply concat
            (for [[key l-column r-column] column-limits :when (< l-column line-length)]
              [key
               (sub-xml-escape-chars (.trim (if (and r-column (< r-column line-length))
                                              (subs line l-column r-column)
                                              (subs line l-column))))])))))))

;(def word-data (mapcat parse-data {gismu-lines gismu-columns, cmavo-lines cmavo-columns}))
(def word-data (mapcat parse-data [[gismu-lines gismu-columns "gismu"]]))

(def split-definitions (partial re-split #"\s*;\s*"))
(def sub-definition-vars (partial re-gsub #"(x\d)" (fn [[_ variable]]
                                                      (str "<var>" variable "</var>"))))
(def transform-definitions (partial map (partial format "<li>%s</li>")))
(def join-definitions (partial str-join "\n"))

(defn- prepare-definition [string]
  (-> string sub-definition-vars split-definitions transform-definitions join-definitions))

(defn- prepare-misc-info [string]
  (if (empty? string) "" (format "<p class=\"note\">%s</p>" string)))

(defn dump-xml [data]
  (println "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  (println "<d:dictionary xmlns=\"http://www.w3.org/1999/xhtml\"")
  (println "  xmlns:d=\"http://www.apple.com/DTDs/DictionaryService-1.0.rng\">\n")
  
  (println "<d:entry id=\"front-back-matter\" d:title=\"(front/back matter)\">")
  (println "<h1>The Lojban Dictionary in English</h1>")
  (println "<p>Based on the word lists from the Logical Language Group of the 1990s</p>")
  (println "</d:entry>")
  
  (doseq [word-datum data]
    (let [type (get-type word-datum)
          word (get-word word-datum)
          stripped-word (re-gsub stop-re "" word)
          keyword (get-keyword word-datum)
          definition (get-definition word-datum)
          frequency (get-frequency word-datum)
          misc-info (get-misc-info word-datum)]
      (printf "<d:entry id=\"%s\" d:title=\"%s\">

%s
<h1>%s</h1>
<p class=\"word-type\">%s</p>
<ul>
%s
</ul>
%s<p class=\"minor-note\">Frequency: %s</p>

</d:entry>
"
        (sub-id-escape-chars word) word
        (str-join "\n"
          (map (partial format "<d:index d:value=\"%s\"/>")
            (remove nil?
              [word
               (if (not= stripped-word word)
                 (str "\n<d:index d:value=\"" stripped-word "\"/>"))
               keyword])))
        word type (prepare-definition definition) (prepare-misc-info misc-info) frequency)))
  (println "</d:dictionary>"))

(dump-xml word-data)














