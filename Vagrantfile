Vagrant.configure('2') do |config|
  config.vm.box_url = 'https://cloud-images.ubuntu.com/vagrant/saucy/current/saucy-server-cloudimg-amd64-vagrant-disk1.box'
  config.vm.box     = 'saucy64'

  config.vm.synced_folder '.', '/home/vagrant/khan'

  config.ssh.forward_agent = true

  config.vm.provider :virtualbox do |vb|
    vb.customize ['modifyvm', :id, '--memory', '4096']
    vb.customize ['modifyvm', :id, '--cpus', '2']
  end

  config.vm.provision :shell, :inline => <<-SCRIPT
updated=~/.apt-updated

if [ ! -f $updated ]; then apt-get update && touch $updated; fi

apt-get install -y \
 build-essential \
 linux-headers-generic \
 man \
 git-core \
 zlib1g-dev \
 ghc \
 cabal-install \
 dkms \
 upx-ucl

su - vagrant -c "
echo 'export PATH=~/.cabal/bin:$PATH' > ~/.bashrc

cabal update && {
    cabal install cabal-install
    cabal install alex happy
}
"
SCRIPT
end
