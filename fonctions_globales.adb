with Ada.Text_IO; use Ada.Text_IO;
with Routeur_exceptions; use Routeur_exceptions;

package body Fonctions_globales is

   -- Fonctions
   function Traiter_c(Arg : in String) return Integer is
   begin
      return Integer'Value(Arg);
   exception
      when Constraint_Error => 
         Put("Erreur : incompréhension après commande -c");
         raise Commande_Inconnu_Error;
   end Traiter_c;


   function Traiter_p (Arg : in String) return Tab_Politique is
   begin
      if Arg = "FIFO" then
         return FIFO;
      elsif Arg = "LRU" then
         return LRU;
      elsif Arg = "LFU" then
         return LFU;
      else
         raise Constraint_Error;
      end if;
   exception
      when Constraint_Error =>
         Put("Erreur : incompréhension après commande -p");
         raise Commande_Inconnu_Error;
   end Traiter_p;


   function Traiter_t (Arg : in String) return String is
   begin
      return Arg;
   exception
      when Constraint_Error => 
         Put("Erreur : incompréhension après commande -t");
         raise Commande_Inconnu_Error;
   end Traiter_t;


   function Traiter_q (Arg : in String) return String is
   begin
      return Arg;
   exception
      when Constraint_Error => 
         Put("Erreur : incompréhension après commmande -q");
         raise Commande_Inconnu_Error;
   end Traiter_q;


   function Traiter_r (Arg: in String) return String is
   begin
      return Arg;
   exception
      when Constraint_Error => 
         Put("Erreur incompréhension après commande -r");
         raise Commande_Inconnu_Error;
   end Traiter_r;


end Fonctions_globales;