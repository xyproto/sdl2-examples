%define _unpackaged_files_terminate_build 1
%define _jvmdir /usr/lib/jvm
%define openjdk_lib java-21-openjdk-21.0.8.0.9-alt1.x86_64/lib/

Name: sdl2-examples
Version: 1.0
Release: alt1

Summary: "hello world" for SDL2 for various programming languages
License: BSD-3-Clause
Group: Development/Other
Url: https://github.com/xyproto/sdl2-examples.git
Vcs: https://github.com/xyproto/sdl2-examples.git

Source0: %name-%version.tar 
Source1: rust-vendor.tar

Patch0: %name-%version-java-makefile.patch

BuildRequires: rust-cargo
BuildRequires: /proc
BuildRequires: golang
BuildRequires: git
BuildRequires: java-21-openjdk-devel
BuildRequires: python3-dev
BuildRequires: python3-module-sdl2
BuildRequires: libSDL2_image-devel
BuildRequires: libSDL2-devel
BuildRequires: gcc-c++
BuildRequires: cmake
BuildRequires: gcc

%description
"hello world" for SDL2 for various programming languages.
Each sample creates a window, displays a grumpy cat image,
then waits two seconds and quits.

All executables should ideally build and run on Linux, macOS,
Windows, BSD* and more, but they should at least work on Linux. 

Most subdirectories contains README.md files with more details,
and a Makefile to have one way of building each sample.

For newer versions of macOS, the programs also appear to need 
an event loop for the window to show up, so I'm in the process 
of adding that to each example. The window just isn't shown if
there is no event loop.

Currently packaged examples:
 - Golang
 - Rust
 - Java
 - Python
 - C++23 CMake
 - C++20 CMake
 - C++17 CMake
 - C++14 CMake
 - C++11 CMake
 - C++11
 - C18
 - C11openjdk_lib

%prep
%setup -a1
%autopatch -p1

mv rust-vendor rust/vendor
pushd rust
mkdir -p .cargo
cat >> .cargo/config.toml <<EOF
[source.crates-io]
replace-with = "vendored-sources"

[source.vendored-sources]
directory = "vendor"

[term]
verbose = true
quiet = false

[install]
root = "%buildroot%_prefix"

[build]
rustflags = ["-Copt-level=3", "-Cdebuginfo=1"]

[profile.release]
strip = false
EOF
popd

sed -i 's|"../img/grumpy-cat.bmp"|"%_datadir/%name/img/grumpy-cat.bmp"|' rust/src/main.rs

sed -i 's|("..", "img", "grumpy-cat.png")|("/", "usr", "share", "sdl2-examples", "img", "grumpy-cat.bmp")|' go/main.go

# https://packages.altlinux.org/en/sisyphus/srpms/python-module-sdl2/
# This module is removed, so i decided to change version of python
sed -i '1s|/usr/bin/env python|/usr/bin/python3|' python/main.py

sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++23-cmake/CMakeLists.txt
sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++20-cmake/CMakeLists.txt
sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++17-cmake/CMakeLists.txt
sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++14-cmake/CMakeLists.txt
sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++11-cmake/CMakeLists.txt

sed -i 's|"../img/grumpy-cat.bmp"|"%_datadir/%name/img/grumpy-cat.bmp"|' c++11/main.cpp

sed -i 's|"../img/grumpy-cat.bmp"|"%_datadir/%name/img/grumpy-cat.bmp"|' c11/main.c
sed -i 's|"../img/grumpy-cat.bmp"|"%_datadir/%name/img/grumpy-cat.bmp"|' c18/main.c

%build

pushd rust
cargo build --release -j12 --offline
popd

pushd go
export GOROOT=/usr/lib/golang
%make_build
popd

pushd java22
export LD_LIBRARY_PATH=%_jvmdir/%openjdk_lib/
%make_build
popd

python3 -m compileall python/

pushd c++23-cmake
%cmake
%cmake_build
popd

pushd c++20-cmake
%cmake
%cmake_build
popd

pushd c++17-cmake
%cmake
%cmake_build
popd

pushd c++14-cmake
%cmake
%cmake_build
popd

pushd c++11-cmake
%cmake
%cmake_build
popd

pushd c++11
%make_build
popd

pushd c18
%make_build
popd

pushd c11
%make_build
popd

%install
mkdir -p \
  %buildroot%_datadir/%name/python \
  %buildroot%_datadir/%name/java \
  %buildroot%_bindir \
  %buildroot%_desktopdir \
  %buildroot%_iconsdir/hicolor/64x64/apps \
  %buildroot%_datadir/%name/img \

install -pm 644 img/grumpy-cat.bmp %buildroot%_datadir/%name/img/

# Rust
install -p -m 755 rust/target/release/rust %buildroot%_bindir/sdl2-rust-example

# Go
install -p -m 755 go/go %buildroot%_bindir/sdl2-go-example

# Java
cp -a java22/* %buildroot%_datadir/%name/java

cat <<'EOF' > %buildroot%_bindir/sdl2-java-example
#!/bin/sh
pushd %_datadir/%name/java
make run
popd
EOF

chmod +x %buildroot%_bindir/sdl2-java-example

# Python
cp -a python/* %buildroot%_datadir/%name/python

cat <<'EOF' > %buildroot%_bindir/sdl2-python-example
#!/bin/sh
pushd %_datadir/%name/python
exec ./main.py
popd
EOF

chmod +x %buildroot%_bindir/sdl2-python-example

# C++23 cmake
install -p -m 755 c++17-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp23-cmake-example

# C++20 cmake
install -p -m 755 c++20-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp20-cmake-example

# C++17 cmake
install -p -m 755 c++17-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp17-cmake-example

# C++14 cmake
install -p -m 755 c++17-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp14-cmake-example

# C++11 cmake
install -p -m 755 c++17-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp11-cmake-example

# C++11
install -p -m 755 c++11/main %buildroot%_bindir/sdl2-cpp11-example

# C18
install -p -m 755 c11/main %buildroot%_bindir/sdl2-c18-example

# C11
install -p -m 755 c11/main %buildroot%_bindir/sdl2-c11-example

# Go
install -p -m 755 go/go %buildroot%_bindir/sdl2-go-example

cat << EOF > %buildroot%_desktopdir/sdl2-rust-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 Rust Example
Exec=sdl2-rust-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-go-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 Go Example
Exec=sdl2-go-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-java-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 Java Example
Exec=sdl2-java-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-python-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 Python Example
Exec=sdl2-python-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp23-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++23 CMake Example
Exec=sdl2-cpp23-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp20-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++20 CMake Example
Exec=sdl2-cpp20-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp17-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++17 CMake Example
Exec=sdl2-cpp17-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp14-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++14 CMake Example
Exec=sdl2-cpp14-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp11-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++11 CMake Example
Exec=sdl2-cpp11-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp11-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++11 Example
Exec=sdl2-cpp11-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-c18-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C18 Example
Exec=sdl2-c18-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-c11-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C11 Example
Exec=sdl2-c11-example
Icon=grumpy-cat
Categories=Other;
EOF

install -pm 644 img/grumpy-cat.png %buildroot%_iconsdir/hicolor/64x64/apps/grumpy-cat.png

%files
%doc README.md LICENSE COMPILES.md

%_datadir/%name/img/
%_desktopdir/*.desktop
%_iconsdir/hicolor/64x64/apps/grumpy-cat.png

%_bindir/sdl2-rust-example

%_bindir/sdl2-go-example

%_bindir/sdl2-java-example
%_datadir/%name/java/

%_bindir/sdl2-python-example
%_datadir/%name/python/

%_bindir/sdl2-cpp23-cmake-example
%_bindir/sdl2-cpp20-cmake-example
%_bindir/sdl2-cpp17-cmake-example
%_bindir/sdl2-cpp14-cmake-example
%_bindir/sdl2-cpp11-cmake-example
%_bindir/sdl2-cpp11-example

%_bindir/sdl2-c11-example
%_bindir/sdl2-c18-example

%changelog
* Mon Jul 21 2025 Fedor Moseichuck <fedor@altlinux.org> 1.0-alt1
- Initial build for Sisyphus.
