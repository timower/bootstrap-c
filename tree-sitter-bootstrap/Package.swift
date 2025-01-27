// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterBootstrap",
    products: [
        .library(name: "TreeSitterBootstrap", targets: ["TreeSitterBootstrap"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterBootstrap",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterBootstrapTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterBootstrap",
            ],
            path: "bindings/swift/TreeSitterBootstrapTests"
        )
    ],
    cLanguageStandard: .c11
)
