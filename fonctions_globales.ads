package Fonctions_globales is
   type Tab_Politique is (FIFO, LRU, LFU);

   type T_Octet is mod 2**8;

  	type T_Adresse_IP is mod 2 ** 32;

	package Octet_IO is new Modular_IO (T_Octet);
	use Octet_IO;

	package Adresse_IP_IO is new Modular_IO (T_Adresse_IP);
	use Adresse_IP_IO;

	UN_OCTET: constant T_Adresse_IP := 2 ** 8;       -- 256
	POIDS_FORT : constant T_Adresse_IP  := 2 ** 31;

   type T_Case is record
      Masque : T_Adresse_IP;
      Interface : String;
   end record;
   
   package LCA_routeur_simple is new LCA (
      T_Cle => T_Adresse_IP,
      T_Valeur => T_Case
   );
   use LCA_routeur_simple;

   function Traiter_c(Arg: in String; Cache : in out Integer) return Integer;

   function Traiter_p (Arg : in String; Politique : in out Tab_Politique) return Tab_Politique;

   function Traiter_t (Arg : in String; Table : in out String) return String;

   function Traiter_q (Arg : in String; Paquet : in out String) return String;

   function Traiter_r (Arg: in String; Resultat : in out String) return String;
end Fonctions_globales;