package Fonctions_globales is
   type Tab_Politique is (FIFO, LRU, LFU);

   function Traiter_c(Arg: in String; Cache : in out Integer) return Integer;

   function Traiter_p (Arg : in String; Politique : in out Tab_Politique) return Tab_Politique;

   function Traiter_t (Arg : in String; Table : in out String) return String;

   function Traiter_q (Arg : in String; Paquet : in out String) return String;

   function Traiter_r (Arg: in String; Resultat : in out String) return String;
end Fonctions_globales;