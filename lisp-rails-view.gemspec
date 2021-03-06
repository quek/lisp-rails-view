# -*- encoding: utf-8 -*-
require File.expand_path('../lib/lisp-rails-view/version', __FILE__)

Gem::Specification.new do |gem|
  gem.authors       = ["TAHARA Yoshinori"]
  gem.email         = ["read.eval.print@gmail.com"]
  gem.description   = %q{lisp-rails-view is a templating engine for HTML.}
  gem.summary       = %q{lisp-rails-view is a templating engine for HTML.}
  gem.homepage      = "https://github.com/quek/lisp-rails-view"

  gem.files         = `git ls-files`.split($\)
  gem.executables   = gem.files.grep(%r{^bin/}).map{ |f| File.basename(f) }
  gem.test_files    = gem.files.grep(%r{^(test|spec|features)/})
  gem.name          = "lisp-rails-view"
  gem.require_paths = ["lib"]
  gem.version       = LispRailsView::VERSION

  gem.add_development_dependency 'bundler'
  gem.add_development_dependency 'rake'
end
