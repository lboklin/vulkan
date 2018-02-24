{ mkDerivation, fetchFromGitHub, base, stdenv, vector-sized, hpack, vulkan-loader }:
mkDerivation {
  pname = "vulkan";
  version = "1.8.0.0";

  src = ./.;

  preConfigure = "${hpack}/bin/hpack";

  libraryHaskellDepends = [ base vector-sized ];
  librarySystemDepends = [ vulkan-loader ];

  homepage = "http://github.com/KaneTW/vulkan#readme";
  description = "Bindings to the Vulkan graphics API";
  license = stdenv.lib.licenses.bsd3;
}
