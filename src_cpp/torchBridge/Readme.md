## Torch Bridge

(Unsupported yet)  
This is a bridge that extends Nerlnet to support libtorch as cpp neural network library.  

### Installation

1. Go to [Pytorch site](https://pytorch.org/get-started/locally/) and download libtorch
2. Extract libotorch to ```/usr/local/lib/libtorch```
3. Execute Nerlnet build with -t=ON or --torch=ON
4. Select worker infrastructe in Nerlplanner as torch.

For apple silicon use [this repo](https://github.com/Nerlnet/libtorch_compiled) to download compiled libtorch.
