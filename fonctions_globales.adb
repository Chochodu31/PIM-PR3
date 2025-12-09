with LCA;

package body Fonctions_globales is

   -- Fonctions
   function Traiter_c(Arg : in String; Cache : in out Integer) return Integer is
   begin
      Cache := Integer(String);
   exception
      when Constraint_Error => Put("Erreur : incompréhension après commande -c");
   end Traiter_c;


   function Traiter_p (Arg : in String; Politique : in out Tab_Politique) return Tab_Politique is
   begin
      Politique := Arg;
   exception
      when Constraint_Error => Put("Erreur : incompréhension après commande -p");
   end Traiter_p;


   function Traiter_t (Arg : in String; Table : in out String) return String is
   begin
      Table := Arg;
   exception
      when Constraint_Error => Put("Erreur : incompréhension après commande -t");
   end Traiter_t;


   function Traiter_q (Arg : in String; Paquet : in out String) return String is
   begin
      Paquet := Arg;
   exception
      when Constraint_Error => Put("Erreur : incompréhension après commmande -q");
   end Traiter_q;


   function Traiter_r (Arg: in String; Resultat : in out String) return String is
   begin
      Resultat := Arg;
   exception
      when Constraint_Error => Put("Erreur incompréhension après commande -r");
   end Traiter_r;


end Fonctions_globales;