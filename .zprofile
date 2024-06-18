# add brew to path
eval "$(/opt/homebrew/bin/brew shellenv)"

# matlab to use AF
export BLAS_VERSION=libmwAF_BLAS_ilp64.dylib

alias matlab="/Applications/MATLAB_R2024a.app/bin/matlab"

# user scripts
export PATH="$PATH:$(du "$HOME/.local/bin" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"

# for proper fortran compilation
export SDKROOT=$(xcrun --sdk macosx --show-sdk-path)
export LIBRARY_PATH="$LIBRARY_PATH:$SDKROOT/usr/lib"
