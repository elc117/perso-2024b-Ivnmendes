```bash
$ sudo mkdir -p /var/lib/mysql
$ sudo chown mysql:mysql /var/lib/mysql
$ sudo usermod -d /var/lib/mysql mysql
$ sudo service mysql restart
```

resolve problemas de mysql no codespaces, solução temporária

usar `$sudo mysql -uroot -p` para acessar o mySQL
