{
  description = "Rust ESP-C3 dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        rustPlatform = pkgs.makeRustPlatform {
          rustc = rust;
          cargo = rust;
        };
        ldproxy-src = pkgs.fetchFromGitHub {
          owner = "esp-rs";
          repo = "embuild";
          rev = "f9ffc4b2400db4827289ba4401092f462e035480";
          hash = "sha256-YH2CPb3uBlPncd+KkP25xhCVvDB7HDxJuSqWOJ1LT3k=";
        };
        ldproxy = rustPlatform.buildRustPackage rec {
          pname = "ldproxy";
          version = "0.3.3";
          buildAndTestSubdir = pname;
          src = ldproxy-src;
          cargoHash = "sha256-AXzDi84q5H6yirVAIAKMrlcd6yZkkaddv7ys72gr178=";
        };
        espflash-src = pkgs.fetchFromGitHub {
          owner = "esp-rs";
          repo = "espflash";
          rev = "v2.1.0";
          hash = "sha256-Nv2/33VYpCkPYyUhlVDYJR1BkbtEvEPtmgyZXfVn1ug=";
        };
        espflash = rustPlatform.buildRustPackage rec {
          pname = "espflash";
          version = "2.1.0";
          buildAndTestSubdir = pname;
          src = espflash-src;
          buildInputs = with pkgs; [
          ] ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
            Security
          ]);
          cargoHash = "sha256-Xj5FVTssC3e+mMhDHmKqV6lUQgaIv3aVc1yewbQSy9E=";
        };
        cargo-espflash = rustPlatform.buildRustPackage rec {
          pname = "cargo-espflash";
          version = "2.1.0";
          buildAndTestSubdir = pname;
          src = espflash-src;
          nativeBuildInputs = with pkgs; [
            pkg-config
          ];
          OPENSSL_NO_VENDOR = 1;
          buildInputs = with pkgs; [
            openssl
            openssl.dev
          ] ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
            Security
            SystemConfiguration
          ]);
          cargoHash = "sha256-FpBc92a2JQHRLe5S6yh3l0FpRI8LpkGGEma/4v5X4xs=";
          doCheck = false;
        };
      in
      with pkgs;
      {
        devShells.default = mkShell {
          name = "esp32c3";
          buildInputs = [
            pkg-config
            libllvm
            libuv
            rust
            ldproxy
            espflash
            cargo-espflash
            cargo-generate
          ];
        };
      }
    );
}
