Vagrant.configure("2") do |config|

  config.vm.define "influxdb" do |machine|
    machine.vm.network "private_network", ip: "192.168.50.115"
    machine.vm.provision :shell, :inline => "apt-get update -y"
    machine.vm.provision :shell, :inline => "apt-get install -y wget"
    machine.vm.provision :shell, :inline => "wget http://s3.amazonaws.com/influxdb/influxdb_latest_amd64.deb"
    machine.vm.provision :shell, :inline => "sudo dpkg -i influxdb_latest_amd64.deb"
    machine.vm.provision :shell, :inline => "sudo /etc/init.d/influxdb start"
    machine.vm.box = "trusty64"
    machine.vm.box_url = "https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-amd64-vagrant-disk1.box"
    machine.vm.provider "virtualbox" do |vbox|
      vbox.customize ["modifyvm", :id, "--memory", "2048"]
    end
  end

end