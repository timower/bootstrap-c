package tree_sitter_brio_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_brio "github.com/tree-sitter/tree-sitter-brio/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_brio.Language())
	if language == nil {
		t.Errorf("Error loading Brio grammar")
	}
}
