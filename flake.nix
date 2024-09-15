{
    inputs = {
        # Here you can adjust the IHP version of your project
        # You can find new releases at https://github.com/digitallyinduced/ihp/releases
        ihp.url = "github:digitallyinduced/ihp/v1.3";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, nixpkgs, self, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint

                        # Extra deps
                        random
                    ];
                };
            };

            # flake.nixosConfigurations."lectorial"  = nixpkgs.lib.nixosSystem {
            #   system = "x86_64-linux";
            #   specialArgs = inputs // {
            #     environment = "production";
            #     ihp-migrate = self.packages.x86_64-linux.migrate;
            #     ihpApp = self.packages.x86_64-linux.default;
            #   };
            #   modules = [
            #     ./nixos/configuration.nix
            #   ];
            # };
            flake.nixosConfigurations."lectorial" = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = inputs;
              modules = [
                  "${nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
                  ihp.nixosModules.appWithPostgres
                  ({ ... }: {
                      security.acme.defaults.email = "me@example.com";

                      services.ihp = {
                          # domain = "myihpapp.com";
                          migrations = ./Application/Migration;
                          schema = ./Application/Schema.sql;
                          fixtures = ./Application/Fixtures.sql;
                          sessionSecret = "xxx";
                      };

                      # Add swap to avoid running out of memory during builds
                      # Useful if your server have less than 4GB memory
                      swapDevices = [ { device = "/swapfile"; size = 8192; } ];

                      # This should reflect the nixos version from the NixOS AMI initally installed
                      # After the initial install, it should not be changed. Otherwise e.g. the postgres
                      # server might need a manual data migration if NixOS changes the default postgres version
                      system.stateVersion = "24.05";
                  })
              ];
          };


        };
}
