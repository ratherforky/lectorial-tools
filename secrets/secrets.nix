# Following this tutorial: https://www.youtube.com/watch?v=XuwgvrriXyk
let
  aws-ec2 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPh7iTMlvx2Pe6SYrplhKKy2Km6IcKZsAySZ0c3p7c+C";
in {
  "admin-password.age".publicKeys = [ aws-ec2 ];
}