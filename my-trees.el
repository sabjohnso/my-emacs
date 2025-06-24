;;; my-racket-extras.el --- Extra Support for Racket -*- lexical-binding: t -*-

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(c "https://github.com/tree-sitter/tree-sitter-c.git")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp.git")
	(cmake "https://github.com/uyha/tree-sitter-cmake.git")
	(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(fortran "https://github.com/stadelmanma/tree-sitter-fortran.git")
	(gitignore "https://github.com/shunsambongi/tree-sitter-gitignore.git")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (llvm "https://github.com/benwilliamgraham/tree-sitter-llvm")))

(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(provide 'my-trees)
