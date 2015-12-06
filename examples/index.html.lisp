(:h1.title "lisp-rails-view Lisp ビューを書く")

(:h2 "イストール")

(:p "あらかじめ SBCL をインストールしておいてください。")

(:h2 "使い方")

(:h3 "基本")

;; CL なのでこんにふうにコメントアウトできる
#+ni
(:pre (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*) (ROOM)))

(:p.warning "b__ は予約語なので使えません。")

"文字列はそのまま出力。<p>"

(:p "タグはキーワードで書く。")

(:p "(setf (readtable-case *readtable*) :preserve) しているので大文字で書くと CL"
    (:br)
    (IF (EVENP (GET-UNIVERSAL-TIME))
        "偶数"
        "奇数"))

(:h3 "エスケープ")

(:p "エスケープされるはず。<script>alert(\"hello\");</script>&\"'")

(:h3 "Ruby のコード")

(:div
 (:p "ねねこ")
 (:p (= @facilities first name))
 (:p (= @facilities.first.name))
 (:p (= @facilities[1] name)))

(:p (= Time.now))
(:p (= Time.now.to_s (\:datetime)))
(:p (= @facilities.map (&\:name)))
(:p (= @facilities map (&\:name) join ("、")))


(:h3 "each")

(:p
 (:pre "(:ul
   (@facilities each (lambda (facility)
                     (:li (= facility.name)))))")
 (:ul
  (@facilities each (lambda (facility)
                      (:li (= facility.name))))))

(:h3 "if")

(:p
 (:pre
  "(if (Time now to_i even?)
     \"偶数\"
     \"奇数\")")
 (if (Time now to_i even?)
     "偶数"
     "奇数"))

(:p
 (:pre
  "(if (Time now to_i even?)
     \"偶数\")")
 (if (Time now to_i even?)
     "偶数"))

(:h3 "render")

(:p (:pre "(= render (\"partial\"))"))
(= render ("partial"))


(:h3 "form")

(= form_for (Facility first)
   (lambda (f)
     (:p "フォームの中です<p>")
     (:p
      (= f label (\:name))
      (= f text_field (\:name))
      (= f submit))))
