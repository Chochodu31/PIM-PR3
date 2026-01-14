with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;


package body LISTES is
   procedure Free is
	new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Liste);


   
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


   procedure association_liste(Liste: in T_Liste; Adresse_IP : in T_Adresse_IP; Association : in out Integer; Int : out T_interface; Adresse : out T_Liste; Masque : out T_Adresse_IP) is
      Actuelle : T_Liste;
      Masque_nouv : T_Adresse_IP;
   begin
      --  New_Line;
      --  Put("=== Association Liste ===");
      Actuelle := Liste;
      Rien_Interface (Int);
      Rien(Masque_nouv);
      while Actuelle /= Null loop
         --  New_Line;
         --  New_Line;
         --  Put("=== Nouvelle itération ===");
         --  New_Line;
         --  Put("Adresse IP : ");
         --  Afficher_Adresse_IP (Adresse_IP);
         --  New_Line;
         --  Put("Masque : ");
         --  Afficher_Adresse_IP (Actuelle.Masque);
         --  New_Line;
         --  Put("And : ");
         --  Afficher_Adresse_IP (Et(Adresse_IP, Actuelle.Masque));
         --  New_Line;
         --  Put("Destination : ");
         --  Afficher_Adresse_IP (Actuelle.Destination);
         --  New_Line;
         if (Et(Adresse_IP,Actuelle.Masque) = Actuelle.Destination) and inf(Masque_nouv, Actuelle.Masque) then
            --  Put("Trouvé : ");
            --  Put("True");
            Association := Association + 1;
            Masque_nouv := Actuelle.Masque;
            Int := Actuelle.Int;
            Adresse := Actuelle;
            Masque := Actuelle.Masque;
         else
            --  Put("False");
            Null;
         end if;
         Actuelle := Actuelle.Suivant;
      end loop;

      --  New_Line;
      --  Put("Association : ");
      --  Put(Association);
      --  New_Line;
      if Association /= 0 then
         --  New_Line;
         --  Put("Passage ici");
         --  New_Line;
         Adresse.Frequence := Adresse.Frequence + 1;
         --  New_Line;
         --  Put("Adresse IP souvenir : ");
         --  Afficher_Adresse_IP (Adresse.Destination);
         --  New_Line;
         --  Put("Masque souvenir: ");
         --  Afficher_Adresse_IP(Adresse.Masque);
         --  New_Line;
      else 
         Null;
      end if;
   end association_liste;


   procedure Dest_Masq_Max(Routage : in T_Liste; Cellule : in T_Liste; Adresse_IP : in T_Adresse_IP; Masque : in out T_Adresse_IP; Destination : out T_Adresse_IP) is
      Actuelle : T_Liste;
      egalite : Boolean;
      bit : Integer;
   begin
      Actuelle := Routage;
      Destination := Et(Adresse_IP, Masque);
      --  New_Line;
      --  Put("Masque : ");
      --  Afficher_Adresse_IP (Masque);
      --  New_Line;
      --  Put("=== Recherche de Masque Max ===");
      while Actuelle /= null loop
         --  New_Line;
         --  New_Line;
         --  Put("=== Iteration de recherche Max ===");
         --  New_Line;
         --  Put("Adresse IP : ");
         --  Afficher_Adresse_IP (Adresse_IP);
         --  New_Line;
         --  Put("Adresse IP Actuelle : ");
         --  Afficher_Adresse_IP (Actuelle.Destination);
         --  New_Line;
         --  Put("Masque : ");
         --  Afficher_Adresse_IP (Masque);
         --  New_Line;
         --  Put("And : ");
         --  Afficher_Adresse_IP (Et(Adresse_IP, Masque));
         --  New_Line;
         --  Put("Destination : ");
         --  Afficher_Adresse_IP (Cellule.Destination);
         --  New_Line;
         if (Et(Actuelle.Destination, Masque) = Cellule.Destination) and not(Cellule.Suivant = Actuelle.Suivant) then
            --  New_Line;
            --  Put("Passage ici : ");
            egalite := True;
            bit := 0;
            while egalite and bit < 32 loop
               --  Put(" === Itération n° : ");
               --  Put(bit);
               bit := bit + 1;
               --  Put(" === ");
               --  New_Line;
               --  Put("Adresse IP : ");
               --  Afficher_Adresse_IP (Adresse_IP);
               --  New_Line;
               --  Put("Adresse IP Actuelle : ");
               --  Afficher_Adresse_IP (Actuelle.Destination);
               --  New_Line;
               --  Put("bit : ");
               --  Put(bit);
               --  New_Line;
               --  Put("Poids Fort : ");
               --  Afficher_Adresse_IP (POIDS_FORT);
               --  New_Line;
               --  Put(" ==== Premier terme ===");
               --  New_Line;
               --  Put("Produit :");
               --  Afficher_Adresse_IP (Produit(Actuelle.Destination, bit));
               --  New_Line;
               --  Put("And : ");
               --  Afficher_Adresse_IP (Et(Produit(Actuelle.Destination, bit), Poids_Fort));
               --  New_Line;
               --  Put(" ==== Deuxième terme ===");
               --  New_Line;
               --  Put("Produit :");
               --  Afficher_Adresse_IP (Produit(Adresse_IP, bit));
               --  New_Line;
               --  Put("And : ");
               --  Afficher_Adresse_IP (Et(Produit(Adresse_IP, bit), Poids_Fort));
               --  New_Line;
               egalite := (Et(Produit(Actuelle.Destination, bit), Poids_Fort) = Et(Produit(Adresse_IP, bit), Poids_Fort));
            end loop;
            Rien(Masque);
            --  New_Line;
            --  Put("Masque : ");
            --  Afficher_Adresse_IP (Masque);
            for i in 1..(bit + 1) loop
               Association_int(Masque, (32 - i));
               --  New_Line;
               --  Put("Masque (iteration): ");
               --  Afficher_Adresse_IP (Masque);
            end loop;
            --  New_Line;
            --  Put("=== Destination === ");
            --  New_Line;
            --  Put("Adresse IP : ");
            --  Afficher_Adresse_IP (Adresse_IP);
            --  New_Line;
            --  Put("Masque : ");
            --  Afficher_Adresse_IP (Masque);
            --  New_Line;
            --  Put("ET : ");
            --  Afficher_Adresse_IP (ET(Adresse_IP, Masque));
            --  New_Line;
            Destination := ET(Adresse_IP,Masque);
         else
            Null;
         end if;
         Actuelle := Actuelle.Suivant;
      end loop;
   end Dest_Masq_Max;


   function Taille(Liste : in T_Liste) return Integer is
      I : Integer;
      Actuelle : T_Liste;
   begin
      I := 0;
      Actuelle := Liste;
      while Actuelle /= Null loop
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
      elsif Actuelle.Suivant = Null then
         Liste := Liste.Suivant;
         Free(Actuelle);
      else
         while Actuelle.Suivant.Suivant /= NUll loop
            Actuelle := Actuelle.Suivant;
         end loop;
         Free(Actuelle.Suivant);
         Actuelle.Suivant := Null;
      end if;
   end Elimination_LRU;


   procedure Elimination_LFU(Liste : in out T_Liste) is
      Actuelle_prec : T_Liste;
      Actuelle : T_Liste;
      Min : Integer;
      Cellule : T_Liste;
      Cellule_prec : T_Liste;
   begin
      Min := Integer'Last;
      Actuelle := Liste;
      Actuelle_prec := Null;

      while Actuelle /= Null loop
         if Actuelle.Frequence < Min then
            Min := Actuelle.Frequence;
            Cellule := Actuelle;
            Cellule_prec := Actuelle_prec;
         else
            Null;
         end if;
         Actuelle_prec := Actuelle;
         Actuelle := Actuelle.suivant;
      end loop;

      if Cellule_prec = Null then
         Liste := Liste.Suivant;
      else
         Cellule_prec.Suivant := Cellule.Suivant;
      end if;
      Free(Cellule);
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
         liste.Frequence := 1;
         liste.Destination := Destination;
         liste.Masque := Masque;
         liste.Int := Int;
		else
			Ajout_routeur(liste.Suivant, Destination, Masque, Int);
		end if;
   end Ajout_routeur;


   function Avoir_Int(Cellule : in T_Liste) return T_interface is
   begin
      return Cellule.Int;
   end Avoir_Int;
end LISTES;