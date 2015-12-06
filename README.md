# lisp-rails-view

lisp-rails-view is a templating engine for HTML.

## Installation

Install SBCL:

    sudo apt-get install sbcl

Add this line to your application's Gemfile:

    gem 'lisp-rails-view'

And then execute:

    $ bundle

## Usage

index.html.lisp

```
(:div#main
 (:h3.title "title")
 (:p#foo.bar "Hello"))
```

see https://github.com/quek/lisp-rails-view/tree/master/examples

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
