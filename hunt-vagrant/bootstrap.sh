#!/bin/bash

apt-get update
apt-get install -y python-software-properties
add-apt-repository -y ppa:hvr/ghc
apt-get update
apt-get install -y git build-essential ghc-7.8.2 cabal-install-1.20 zlib1g-dev libbz2-dev libsnappy-dev

P="/opt/cabal/1.20/bin:/opt/ghc/7.8.2/bin:$PATH"

export PATH="$P"
echo "export PATH='$P'" >> /home/vagrant/.bashrc
echo "export PATH='$P'" >> /home/vagrant/.profile

cabal update && cd /hunt/ && make install
