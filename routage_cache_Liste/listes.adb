with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body LISTES is
   procedure Free is
	new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Liste);
   
   type T_Octet is mod 2 ** 8;
   
   procedure Initialiser(liste : out T_Liste) is 
   begin
      liste := Null;
   end Initialiser;


   function Est_Vide(liste: in T_Liste) return Boolean is 
   begin
      return liste = Null;
   end Est_Vide;


	procedure Detruire (liste : in out T_Liste) is
		Nouv_liste : T_Liste;
	begin
		while liste /= Null loop
			Nouv_liste := liste;
			liste := liste.All.Suivant;
			Free (Nouv_liste);
		end loop;
		Free (liste);
	end Detruire;


   function association_liste(Liste: in T_Liste; Adresse_IP : in T_Adresse_IP; Association : in out Integer) return T_interface is
   begin

   end association_liste;


   procedure Ajout_cache (Routage: in T_Liste; Adresse_IP : in T_Adresse_IP; Cache : in out T_Liste; politique : in Tab_Politique; Cache_Taille : in Integer) is
   begin

   end Ajout_cache;


   procedure Afficher_Liste(Liste : in T_Liste) is
      Actuelle : T_Liste;
   begin
      Actuelle := Liste;
      while Actuelle /= Null loop
         Put("-->[Destination : ");
         Afficher_Adresse_IP (Actuelle.Destination);
         Put(" , Masque : ");
         Afficher_Adresse_IP (Actuelle.Masque);
         Put(" , Interface : ");
         Put(Actuelle.Int);
         Put(" , frequence : ");
         Put(Actuelle.Frequence);
         Put("]");
         New_Line;
         Actuelle := Actuelle.Suivant;
      end loop;
      Put("--E");
   end Afficher_Liste;


   procedure Ajout_routeur(Liste : in out T_Liste; Destination : in T_Adresse_IP; Masque : in T_Adresse_IP; Int : T_interface) is
   begin
      if liste = Null then
			liste := new T_Cellule;
			liste.Suivant := Null;
         liste.Frequence := 0;
         liste.Destination := Destination;
         liste.Masque := Masque;
         liste.Int := Int;
		else
			Ajout_routeur(liste.Suivant, liste.Frequence, Destination, Masque, Int);
		end if;
   end Ajout_routeur;

end LISTES;