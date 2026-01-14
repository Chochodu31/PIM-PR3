
generic
   type T_interface is private;
   type T_Adresse_IP is private;
   POIDS_FORT : in T_Adresse_IP;

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

   generic
      with procedure Afficher_Adresse_IP (Adresse_IP : in T_Adresse_IP);
      with procedure Afficher_Int(Int : in T_interface);
   procedure Afficher_Liste (Liste : in T_Liste);

   procedure Ajout_routeur(Liste : in out T_Liste; Destination : in T_Adresse_IP; Masque : in T_Adresse_IP; Int : T_interface);

   procedure Ordre(Cache : in out T_Liste; Cellule : in out T_Liste; politique : in Tab_Politique);

   generic
      with function Et(Adresse_IP : in T_Adresse_IP; Masque : in T_Adresse_IP) return T_Adresse_IP;
      with function Produit(Destination : in T_Adresse_IP; Int : in Integer) return T_Adresse_IP;
      with procedure Association_int(Masque : in out T_Adresse_IP; Int : in Integer);
      with procedure Rien(Masque : in out T_Adresse_IP);
   procedure Dest_Masq_Max(Routage : in T_Liste; Cellule : in T_Liste; Adresse_IP : in T_Adresse_IP; Masque : in out T_Adresse_IP; Destination : out T_Adresse_IP);

   -- Enregistrer une valeur dans la liste
   --  procedure Enregistrer_routage(liste : in out T_Liste; Frequence : Integer; Destination : T_Adresse_IP; Masque : T_Adresse_IP; Int : T_interface);
   generic
      with function Et(Adresse_IP : in T_Adresse_IP; Masque : in T_Adresse_IP) return T_Adresse_IP;
      with function inf(Masque_nouv : in T_Adresse_IP; Masque : in T_Adresse_IP) return Boolean;
      with procedure Rien(Masque : in out T_Adresse_IP);
      with procedure Rien_Interface(Inter : in out T_interface);
   procedure association_liste(Liste: in T_Liste; Adresse_IP : in T_Adresse_IP; Association : in out Integer; Int : out T_interface; Adresse : out T_Liste; Masque : out T_Adresse_IP);
   
   procedure Elimination(Liste: in out T_Liste; politique : in Tab_Politique);
   
   function Taille(Liste : in T_Liste) return Integer;

   function Avoir_Int(Cellule : in T_Liste) return T_interface;
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