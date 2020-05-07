It is a research project which is not going to be used in production at all independently of its state of mature. It is expected that the results of the research will be the base for implementing an alternative to the modern software development systems (so-called programming languages). Each new feature should satisfy restrictions defined in Aspectation Manifesto (https://github.com/jinnzest/aspectation-manifesto).

The Aspectation prototype is dual-licensed under Apache 2.0 and MIT.

See LICENSE-APACHE, LICENSE-MIT, and COPYRIGHT for details.

# LLVM installation

macOS&Linux: export LLVM_PATH=/usr/local/opt/llvm-9

git clone https://github.com/llvm/llvm-project.git 
cd llvm-project
git checkout llvmorg-9.0.1
mkdir tmp
cd tmp
cmake -DCMAKE_INSTALL_PREFIX=$LLVM_PATH -DLLVM_BUILD_LLVM_DYLIB=True -DLLVM_LINK_LLVM_DYLIB=True -G Ninja ../llvm
cmake --build .
cmake --build . --target install

BASH:
echo "export LD_LIBRARY_PATH=$LLVM_PATH/lib:$LD_LIBRARY_PATH" >> ~/.bash_profile
echo "export PATH=$LLVM_PATH/bin:$PATH" >> ~/.bash_profile
ZSH: 
echo "export LD_LIBRARY_PATH=$LLVM_PATH/lib:$LD_LIBRARY_PATH" >> ~/.zshrc
echo "export PATH=$LLVM_PATH/bin:$PATH" >> ~/.zshrc

MacOS Only: 
cd $LLVM_PATH/lib
ln -s libLLVM.dylib libLLVM-9.dylib
install_name_tool -id $PWD/libLTO.dylib libLTO.dylib
install_name_tool -id $PWD/libLLVM.dylib libLLVM.dylib
install_name_tool -change '@rpath/libLLVM.dylib' $PWD/libLLVM.dylib libLTO.dylib


# prepare formatting script

stack install brittany

# prepare hlint script

stack install hlint

# prepare tommath library

git submodule init && git submodule update
cd libtommath; make; 
mac os: cc -fpic -shared -Wl,-all_load libtommath.a -Wl -o libtommath.dylib
linux: cc -fpic -shared -W libtommath.a -W -o libtommath.dylib
cd ..

# build the prototype

stack build

# testing

stack test


# run a single test

stack test --ta ' -p "test name"'