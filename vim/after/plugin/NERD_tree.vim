if exists("loaded_nerd_tree")
    "echo 'loaded_nerd_tree' . loaded_nerd_tree
	"autopen NERDTree and focus cursor in new document  
	autocmd VimEnter * NERDTree  
	autocmd VimEnter * wincmd p
endif