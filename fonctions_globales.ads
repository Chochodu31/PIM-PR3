package Fonctions_globales is
   type Tab_Politique is (FIFO, LRU, LFU);

   function Traiter_c(Arg: in String) return Integer;

   function Traiter_p (Arg : in String) return Tab_Politique;

   function Traiter_t (Arg : in String) return String;

   function Traiter_q (Arg : in String) return String;

   function Traiter_r (Arg: in String) return String;
end Fonctions_globales;