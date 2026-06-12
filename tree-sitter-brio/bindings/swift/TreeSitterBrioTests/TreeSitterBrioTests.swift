import XCTest
import SwiftTreeSitter
import TreeSitterBrio

final class TreeSitterBrioTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_brio())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Brio grammar")
    }
}
