generic
   type T_interface is private;
   type T_Adresse_IP is private;

package LISTES is
   type Tab_Politique is (FIFO, LRU, LFU);
   type T_Liste is limited private;


   -- Initialiser une liste. La liste est vide.
   procedure Initialiser(liste : out T_Liste) with
      Post => Est_Vide(liste);


   -- Detruire une liste. Elle ne devra plus être utilisée.
   procedure Detruire(liste : in out T_Liste);


   -- Est-ce qu'une liste est vide ? 
   function Est_Vide(liste: in T_Liste) return Boolean;


   -- Enregistrer une valeur dans la liste
   --  procedure Enregistrer_routage(liste : in out T_Liste; Frequence : Integer; Destination : T_Adresse_IP; Masque : T_Adresse_IP; Int : T_interface);
   function association_liste(Liste: in T_Liste; Adresse_IP : in T_Adresse_IP; Association : in out Integer) return T_interface;
   
   procedure Ajout_cache(Routage: in T_Liste; Adresse_IP : in T_Adresse_IP; Cache : in out T_Liste; politique : in Tab_politique; Cache_Taille : in Integer);
   
   generic
      with procedure Afficher_Adresse_IP (Adresse_IP : in T_Adresse_IP);
   procedure Afficher_Liste (Liste : in T_Liste);

   procedure Ajout_routeur(Liste : in out T_Liste; Destination : in T_Adresse_IP; Masque : in T_Adresse_IP; Int : T_interface);

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