with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;


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


   procedure association_liste(Liste: in T_Liste; Adresse_IP : in T_Adresse_IP; Association : in out Integer; Int : out T_interface; Adresse : out T_Liste) is
      Actuelle : T_Liste;
      Masque_nouv : T_Adresse_IP;
   begin
      Actuelle := Liste;
      Int := Liste.Int;
      while Actuelle /= Null loop
         if (Et(Adresse_IP,Liste.Masque) = Liste.Destination) and inf(Masque_nouv, Liste.Masque) then
            Association := Association + 1;
            Masque_nouv := Liste.Masque;
            Int := Liste.Int;
            Adresse := Liste;
         else
            Null;
         end if;
         Actuelle := Actuelle.Suivant;
      end loop;
      
      if Association = 0 then
         Adresse.Frequence := Adresse.Frequence + 1;
      else 
         Null;
      end if;
   end association_liste;


   procedure Dest_Masq_Max(Routage : in T_Liste; Cellule : in T_Liste; Adresse_IP : in T_Adresse_IP; Masque : out T_Adresse_IP; Destination : out T_Adresse_IP) is
      Actuelle : T_Liste;
      egalite : Boolean;
      bit : Integer;
   begin
      Actuelle := Routage;
      Masque := Cellule.Masque;
      while Actuelle /= null loop
         if (Et(Actuelle.Destination, Masque) = Cellule.Destination) and not(Cellule.Suivant = Actuelle.Suivant) then
            egalite := True;
            bit := 0;
            while egalite and bit < 32 loop
               bit := bit + 1;
               egalite := (Et(Produit(Actuelle.Destination, bit), Poids_Fort) = Et(Produit(Adresse_IP, bit), Poids_Fort));
            end loop;
            Rien(Masque);
            for i in 1..bit loop
               Association_int(Masque, (32 - i));
            end loop;
            Destination := ET(Adresse_IP,Masque);
         else
            Null;
         end if;
      end loop;
   end Dest_Masq_Max;


   function Taille(Liste : in T_Liste) return Integer is
      I : Integer;
      Actuelle : T_Liste;
   begin
      I := 0;
      Actuelle := Liste;
      while Actuelle = Null loop
         I := I + 1;
         Actuelle := Actuelle.Suivant;
      end loop;
      return I;
   end Taille;


   procedure Ordre(Cache : in out T_Liste; Cellule : in out T_Liste; politique : in Tab_Politique) is
      Actuelle : T_Liste;
   begin
      if Politique = LRU then
         Actuelle := Cache;
         if Cache = Cellule then
            Cache := Cache.Suivant;
         else
            while Actuelle.Suivant /= Cellule loop
               Actuelle := Actuelle.Suivant;
            end loop;
            Actuelle.Suivant := Actuelle.Suivant.Suivant;
         end if;
         Ajout_routeur (Cache, Cellule.Destination, Cellule.Masque, Cellule.Int);
         Free(Cellule);
      else
         null;
      end if;
   end Ordre;

   procedure Elimination_FIFO(Liste : in out T_Liste) is 
      Cellule : T_Liste;
   begin
      Cellule := Liste;
      Liste := Liste.Suivant;
      Free(Cellule);
   end Elimination_FIFO;


   procedure Elimination_LRU(Liste : in out T_Liste) is
      Actuelle : T_Liste;
   begin
      Actuelle := Liste;
      if Actuelle = NUll then
         Null;
      else
         while Actuelle.Suivant /= NUll loop
            Actuelle := Actuelle.Suivant;
         end loop;
         Free(Actuelle.Suivant);
         Actuelle.Suivant := Null;
      end if;
   end Elimination_LRU;


   procedure Elimination_LFU(Liste : in out T_Liste) is
      Cellule_prec : T_Liste;
      Actuelle : T_Liste;
      Min : Integer;
      Cellule : T_Liste;
   begin
      Actuelle := Liste;
      if Actuelle = null then
         null;
      else
         while Actuelle.Suivant /= null loop
            if Actuelle.Suivant.Frequence < Min then
               Min := Actuelle.Suivant.Frequence;
               Cellule_prec := Actuelle;
            else
               null;
            end if;
         end loop;
         Cellule := Cellule_prec.Suivant;
         Cellule_prec.Suivant := Cellule.suivant;
         Free(Cellule);
         end if;
   end Elimination_LFU;


   procedure Elimination(Liste: in out T_Liste; politique : in Tab_Politique) is
   begin
      if Politique = FIFO then
         Elimination_FIFO(Liste);
      elsif Politique = LRU then
         Elimination_LRU(Liste);
      else
         Elimination_LFU(Liste);
      end if;
   end Elimination;






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
         Afficher_Int(Actuelle.Int);
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
			Ajout_routeur(liste.Suivant, Destination, Masque, Int);
		end if;
   end Ajout_routeur;

end LISTES;