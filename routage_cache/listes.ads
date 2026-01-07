with Fonctions_globales;
generic
   type T_interface is private;
   type T_Adresse_IP is private;

package LISTES is
   type T_Liste is limited private;


   -- Initialiser une liste. La liste est vide.
   procedure Initialiser(liste : out T_Liste) with
      Post => Est_Vide(liste);


   -- Detruire une liste. Elle ne devra plus être utilisée.
   procedure Detruire(liste : in out T_Liste);


   -- Est-ce qu'une liste est vide ? 
   function Est_Vide(liste: in T_Liste) return Boolean;


   -- Enregistrer une valeur dans la liste
   procedure Enregistrer_routage(liste : in out T_Liste; Frequence : Integer; Destination : T_Adresse_IP; Masque : T_Adresse_IP; Int : T_interface);


private
   type T_Cellule;

   type T_Liste is access T_Cellule;

   type T_Cellule is record
      Suivant : T_Liste;
      Frequence : Integer;
      Destination : T_Adresse_IP; -- PROBLEME: FICHIERS QUI S'IMPLIQUENT MUTUELLEMENT : A REGLER!!!!!
      Masque : T_Adresse_IP;
      Int : T_interface;
   end record;
end LISTES;