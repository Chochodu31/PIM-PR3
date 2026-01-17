with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with LISTES;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

procedure Tester_LISTES is

	generic
		type K is private;	-- le type de l'interface
		type V is private;	-- le type des adresses_IP
    	Poids_Fort : in V;
		K1, K2, K3, K4 : K;		-- 5 interfaces
		V1, V2, V3, V4 : V;		-- 4 adresses_IP différentes
		A1, A2, A3, A4 : V;
		B1, B2, B3: V; 	-- Masque max attendu
		C1, C2, C3, C4 : V;
		with package P_LISTES is new LISTES (K, V, Poids_Fort);
	
   	package Testeur is

		-- Lancer tous les tests.
		generic
			with function Et_Gen(Adresse_IP : in V; Masque : in V) return V;
			with function Produit_Gen(Destination : in V; Int : in Integer) return V;
			with procedure Association_int_Gen(Masque : in out V; Int : in Integer);
			with procedure Rien_Gen(Masque : in out V);
			with function inf_Gen(Masque_nouv : in V; Masque : in V) return Boolean;
			with procedure Rien_K(Int : in out K);
			with procedure Afficher_Ad_IP_Gen(M1 : in V);
		procedure Tester_Tout_Gen;

		-- Tester Initialiser, Est_Vide, Taille et Detruire (partiel).
		procedure Tester_Initialiser;

      	procedure Tester_Ajout_Routeur;

		generic
			with function Et_Gen(Adresse_IP : in V; Masque : in V) return V;
         	with function inf_Gen(Masque_nouv : in V; Masque : in V) return Boolean;
         	with procedure Rien_Gen(Masque : in out V);
         	with procedure Rien_K(Int : in out K);
      	procedure Tester_Ordre_Gen;

      	generic
        	   with function Et_Gen(Adresse_IP : in V; Masque : in V) return V;
         	with function Produit_Gen(Destination : in V; Int : in Integer) return V;
         	with procedure Association_int_Gen(Masque : in out V; Int : in Integer);
         	with procedure Rien_Gen(Masque : in out V);
      	procedure Tester_Dest_Masq_Max_Gen;

      	generic
         	with function Et_Gen(Adresse_IP : in V; Masque : in V) return V;
         	with function inf_Gen(Masque_nouv : in V; Masque : in V) return Boolean;
         	with procedure Rien_Gen(Masque : in out V);
         	with procedure Rien_K(Int : in out K);
      	procedure Tester_association_liste_Gen;

      	procedure Tester_Elimination_FIFO;

         generic
            with function Et_Gen(Adresse_IP : in V; Masque : in V) return V;
         	with function inf_Gen(Masque_nouv : in V; Masque : in V) return Boolean;
         	with procedure Rien_Gen(Masque : in out V);
         	with procedure Rien_K(Int : in out K);
            with procedure Afficher_Ad_IP_Gen(M1: in V);
      	procedure Tester_Elimination_LFU_Gen;

      	procedure Tester_Elimination_LRU;

	end Testeur;


	package body Testeur is
		use P_LISTES;


		procedure Tester_Tout_Gen is
			procedure Tester_Ordre is new Tester_Ordre_Gen(
				Et_Gen => Et_Gen,
				inf_Gen => inf_Gen,
				Rien_Gen => Rien_Gen,
				Rien_K => Rien_K
			);

         procedure Tester_Elimination_LFU is new Tester_Elimination_LFU_Gen(
				Et_Gen => Et_Gen,
				inf_Gen => inf_Gen,
				Rien_Gen => Rien_Gen,
				Rien_K => Rien_K,
				Afficher_Ad_IP_Gen => Afficher_Ad_IP_Gen
			);

			procedure Tester_Dest_Masq_Max is new Tester_Dest_Masq_Max_Gen(
				Et_Gen => Et_Gen,
				Produit_Gen => Produit_Gen,
				Association_int_Gen => Association_int_Gen,
				Rien_Gen => Rien_Gen
			);

			procedure Tester_association_liste is new Tester_association_liste_Gen(
				Et_Gen => Et_Gen,
				inf_Gen => inf_Gen,
				Rien_Gen => Rien_Gen,
				Rien_K => Rien_K
			);

		begin
			Tester_Initialiser;
			Tester_Ajout_Routeur;
			Tester_Ordre;
			Tester_Dest_Masq_Max;
			Tester_association_liste;
			Tester_Elimination_FIFO;
			Tester_Elimination_LFU;
			Tester_Elimination_LRU;
			Put_Line ("Tests : OK.");
		end Tester_Tout_Gen;


		procedure Tester_Initialiser is
			Liste : T_Liste;
		begin
			Put ("Tester_Initialiser : ");
			Initialiser (Liste);
			pragma assert (Est_Vide (Liste));
			pragma assert (Taille (Liste) = 0);
			Detruire (Liste);
			Put_Line ("OK");
		end Tester_Initialiser;


		procedure Tester_Ajout_Routeur is
			Liste : T_Liste;
			Cellule1 : T_Liste;
			Cellule2 : T_Liste;
			Cellule3 : T_Liste;
			Cellule4 : T_Liste;
		begin
			Put("Tester_Ajout_Routeur : ");
			Initialiser (Liste);
			Initialiser(Cellule1);
			Ajout_routeur (Liste, V1, A1, K1);
			Ajout_routeur(Cellule1, V1, A1, K1);
			pragma assert (Presence_fin (Liste, Cellule1));
			pragma assert (Presence(Liste, Cellule1));

			Initialiser(Cellule2);
			Ajout_routeur (Cellule2, V2, A2, K2);
			Ajout_routeur (Liste, V2, A2, K2);
			pragma assert (Presence(Liste, Cellule1));
			pragma assert(Presence(Liste, Cellule2));
			pragma assert(Presence_fin (Liste, Cellule2));

			Initialiser(Cellule3);
			Ajout_routeur (Cellule3, V3, A3, K3);
			Ajout_routeur (Liste, V3, A3, K3);
			pragma assert (Presence(Liste, Cellule1));
			pragma assert(Presence(Liste, Cellule2));
			pragma assert(Presence(Liste, Cellule3));
			pragma assert(Presence_fin (Liste, Cellule3));

			Initialiser(Cellule4);
			Ajout_routeur (Cellule4, V4, A4, K4);
			Ajout_routeur (Liste, V4, A4, K4);
			pragma assert (Presence(Liste, Cellule1));
			pragma assert(Presence(Liste, Cellule2));
			pragma assert(Presence(Liste, Cellule3));
			pragma assert(Presence(Liste, Cellule4));
			pragma assert(Presence_fin (Liste, Cellule4));

			Detruire(Liste);
			Detruire(Cellule1);
			Detruire(Cellule2);
			Detruire(Cellule3);
			Detruire(Cellule4);
			Put_Line ("OK");
		end Tester_Ajout_Routeur;


		procedure Creer_Liste(Liste : in out T_Liste) is
		begin
			Initialiser (Liste);
			Ajout_routeur (Liste, V1, A1, K1);
			Ajout_routeur (Liste, V2, A2, K2);
			Ajout_routeur (Liste, V3, A3, K3);
			Ajout_routeur (Liste, V4, A4, K4);
		end Creer_Liste;
		
		procedure Tester_Ordre_Gen is
			procedure Association_liste_Det is new Association_liste(
				Et => Et_Gen,
				inf => inf_Gen,
				Rien => Rien_Gen,
				Rien_Interface => Rien_K
			);

			Liste : T_Liste;
			cellule1 : T_Liste;
			cellule2 : T_Liste;
			cellule3 : T_Liste;
			Cellule4 : T_Liste;
			Adresse : T_Liste;
			Association : Integer := 0;
			Int : K;
			Masque_out : V;
		begin
			Creer_Liste(Liste);
			Put("Tester_Ordre : ");
			Ajout_routeur (cellule1, V1, A1, K1);
			Ajout_routeur (cellule2, V2, A2, K2);
			Ajout_routeur (cellule3, V3, A3, K3);
			Ajout_routeur (cellule4, V4, A4, K4);
			pragma assert (Presence(Liste, cellule1));
			pragma assert (Presence(Liste, cellule2));
			pragma assert (Presence(Liste, cellule3));
			pragma assert (Presence(Liste, cellule4));
			pragma assert (Presence_fin(Liste, cellule4));

			Association_liste_Det (Liste, V1, Association, Int, Adresse, Masque_out);
			Ordre(liste, Adresse, FIFO);
			pragma assert (Presence(Liste, cellule1));
			pragma assert (Presence(Liste, cellule2));
			pragma assert (Presence(Liste, cellule3));
			pragma assert (Presence(Liste, cellule4));
			pragma assert (Presence_fin (Liste, Cellule4));

			Ordre(liste, Adresse, LFU);
			pragma assert (Presence(Liste, cellule1));
			pragma assert (Presence(Liste, cellule2));
			pragma assert (Presence(Liste, cellule3));
			pragma assert (Presence(Liste, cellule4));
			pragma assert(Presence_fin(Liste, Cellule4));

			Ordre(liste, Adresse, LRU);
			pragma assert (Presence(Liste, cellule1));
			pragma assert (Presence(Liste, cellule2));
			pragma assert (Presence(Liste, cellule3));
			pragma assert (Presence(Liste, cellule4));
			pragma assert (Presence_fin(Liste, cellule1));

			Detruire(Liste);
			Detruire(cellule1);
			Detruire(cellule2);
			Detruire(cellule3);
			Detruire(cellule4);
			Put_line("OK");
		end Tester_Ordre_Gen;

		procedure Creer_Liste_Nouv(Liste : in out T_Liste) is
		begin
			Initialiser (Liste);
			Ajout_routeur (Liste, V1, A1, K1);
			Ajout_routeur (Liste, V2, A2, K2);
			Ajout_routeur (Liste, V3, A3, K3);
		end Creer_Liste_Nouv;


		procedure Tester_Dest_Masq_Max_Gen is
			procedure Dest_Masq_Max_Det is new Dest_Masq_Max(
				Et => Et_Gen,
				Produit => Produit_Gen,
				Association_Int => Association_int_Gen,
				Rien => Rien_Gen
			);


			Liste : T_Liste;
			cellule1 : T_Liste;
			cellule2 : T_Liste;
			cellule3 : T_Liste;
			Masque : V;
			Destination : V;
		begin
			Put("Tester_Dest_Masq_Max : ");
			Ajout_routeur (cellule1, V1, A1, K1);
			Ajout_routeur (cellule2, V2, A2, K2);
			Ajout_routeur (cellule3, V3, A3, K3);
			Creer_Liste_Nouv (Liste);
			Masque := A1;
			Dest_Masq_Max_Det (Liste, cellule1, V1, Masque, Destination);
			pragma assert(Masque = B1);
			pragma assert(Destination = Et_Gen(V1, Masque));

			Dest_Masq_Max_Det(Liste, cellule2, V2, Masque, Destination);
			Masque := A2;
			pragma assert(Masque = B2);
			pragma assert(Destination = Et_Gen (V2,Masque));

			Dest_Masq_Max_Det(Liste, cellule3, V3, Masque, Destination);
			Masque := A3;
			pragma assert(Masque = B3);
			pragma assert(Destination = Et_Gen (V3,Masque));

			
			Detruire(Liste);
			Put_line("OK");
		end Tester_Dest_Masq_Max_Gen;


		procedure Tester_association_liste_Gen is
			procedure Association_liste_Det is new Association_liste(
				Et => Et_Gen,
				inf => inf_Gen,
				Rien => Rien_Gen,
				Rien_Interface => Rien_K
			);

			Liste : T_Liste;
			Association : Integer;
			Int : K;
			Adresse : T_Liste;
			Masque : V;
		begin
			Put("Tester_association_liste : ");
			Creer_Liste_Nouv (Liste);

			Association := 0;
			Association_liste_Det (Liste, C1, Association, Int, Adresse, Masque);
			pragma assert (V1 = Avoir_Des (Adresse));
			pragma assert (Masque = B1);
			pragma assert (Association /= 0);
			pragma assert (Int = K1);

			Association := 0;
			Association_liste_Det (Liste, C2, Association, Int, Adresse, Masque);
			pragma assert (V2 = Avoir_Des (Adresse));
			pragma assert (Masque = B2);
			pragma assert (Association /= 0);
			pragma assert (Int = K2);

			Association := 0;
			Association_liste_Det (Liste, C3, Association, Int, Adresse, Masque);
			pragma assert (V3 = Avoir_Des (Adresse));
			pragma assert (Masque = B3);
			pragma assert (Association /= 0);
			pragma assert (Int = K3);

			Association := 0;
			Association_liste_Det (Liste, C4, Association, Int, Adresse, Masque);
			pragma assert (Association = 0);

			Detruire(Liste);
			Put_line("OK");
		end Tester_association_liste_Gen;


		procedure Tester_Elimination_FIFO is
         Liste : T_Liste;
         cellule1 : T_Liste;
			cellule2 : T_Liste;
			cellule3 : T_Liste;
			Cellule4 : T_Liste;
      begin
			Put("Tester_Elimination_FIFO : ");
         Ajout_routeur (cellule1, V1, A1, K1);
			Ajout_routeur (cellule2, V2, A2, K2);
			Ajout_routeur (cellule3, V3, A3, K3);
			Ajout_routeur (cellule4, V4, A4, K4);
         Creer_Liste(Liste);
         pragma assert(Presence(Liste, cellule1));
         pragma assert(Presence(Liste, cellule2));
         pragma assert(Presence(Liste, cellule3));
         pragma assert(Presence(Liste, cellule4));

         Elimination_FIFO (Liste);
         pragma assert(not Presence(Liste, cellule1));
         pragma assert(Presence(Liste, cellule2));
         pragma assert(Presence(Liste, cellule3));
         pragma assert(Presence(Liste, cellule4));

         Detruire(Liste);
         Detruire(Cellule1);
         Detruire(Cellule2);
         Detruire(Cellule3);
         Detruire(Cellule4);
			Put_line("OK");
		end Tester_Elimination_FIFO;


		procedure Tester_Elimination_LFU_Gen is 
         	procedure Association_liste_Det is new Association_liste(
				Et => Et_Gen,
				inf => inf_Gen,
				Rien => Rien_Gen,
				Rien_Interface => Rien_K
			);

         	Liste : T_Liste;
         	cellule1 : T_Liste;
			cellule2 : T_Liste;
			cellule3 : T_Liste;
			Cellule4 : T_Liste;
	        Association : Integer := 0;
         	Int : K;
         	Adresse : T_Liste;
         	Masque : V;
      	begin
        	Put("Tester_Elimination_LFU : ");
        	Ajout_routeur (cellule1, V1, A1, K1);
			Ajout_routeur (cellule2, V2, A2, K2);
			Ajout_routeur (cellule3, V3, A3, K3);
			Ajout_routeur (cellule4, V4, A4, K4);
         	Creer_Liste(Liste);
         	pragma assert(Presence(Liste, cellule1));
         	pragma assert(Presence(Liste, cellule2));
         	pragma assert(Presence(Liste, cellule3));
         	pragma assert(Presence(Liste, cellule4));
         	Association_liste_Det (Liste, V1, Association, Int, Adresse, Masque);
         	Association_liste_Det (Liste, V2, Association, Int, Adresse, Masque);
			Association_liste_Det (Liste, V4, Association, Int, Adresse, Masque);
         	Elimination_LFU (Liste);
         	pragma assert(Presence(Liste, cellule1));
         	pragma assert(Presence(Liste, cellule2));
         	pragma assert(not Presence(Liste, cellule3));
         	pragma assert(Presence(Liste, cellule4));

         	Detruire(Liste);
         	Detruire(Cellule1);
         	Detruire(Cellule2);
         	Detruire(Cellule3);
         	Detruire(Cellule4);
			Put_line("OK");
		end Tester_Elimination_LFU_Gen;

		
		procedure Tester_Elimination_LRU is
         	Liste : T_Liste;
         	cellule1 : T_Liste;
			cellule2 : T_Liste;
			cellule3 : T_Liste;
			Cellule4 : T_Liste;
      	begin
		Put("Tester_Elimination_LRU : ");
        Ajout_routeur (cellule1, V1, A1, K1);
		Ajout_routeur (cellule2, V2, A2, K2);
		Ajout_routeur (cellule3, V3, A3, K3);
		Ajout_routeur (cellule4, V4, A4, K4);
        Creer_Liste(Liste);
        pragma assert(Presence(Liste, cellule1));
        pragma assert(Presence(Liste, cellule2));
        pragma assert(Presence(Liste, cellule3));
        pragma assert(Presence(Liste, cellule4));

        Elimination_FIFO (Liste);
        pragma assert(not Presence(Liste, cellule1));
        pragma assert(Presence(Liste, cellule2));
        pragma assert(Presence(Liste, cellule3));
        pragma assert(Presence(Liste, cellule4));

         Detruire(Liste);
         Detruire(Cellule1);
         Detruire(Cellule2);
         Detruire(Cellule3);
         Detruire(Cellule4);
			Put_line("OK");
		end Tester_Elimination_LRU;

	end Testeur;


	type T_Adresse_IP is mod 2 ** 32;
	UN_OCTET: constant T_Adresse_IP := 2 ** 8;
	POIDS_FORT_ICI : constant T_Adresse_IP  := 2 ** 31;

	
	package LISTES_Adresse_IP is new LISTES (
		T_interface => Unbounded_String, 
		T_Adresse_IP => T_Adresse_IP, 
		POIDS_FORT => POIDS_FORT_ICI
	
	);


	function Transfo_Ad_IP(B1, B2, B3, B4 : in T_Adresse_IP) return T_Adresse_IP is 
   	begin
      	return (B1 * UN_OCTET**3) + (B2 * UN_OCTET**2) + (B3 * UN_OCTET) + B4;
	end Transfo_Ad_IP;


	Ad_IP1 : constant T_Adresse_IP := Transfo_Ad_IP (128, 255, 86, 70);
	Ad_IP2 : constant T_Adresse_IP := Transfo_Ad_IP (128, 255, 86, 75);
	Ad_IP3 : constant T_Adresse_IP := Transfo_Ad_IP (70, 87, 1, 0);
	Ad_IP4 : constant T_Adresse_IP := Transfo_Ad_IP (0, 0, 0, 0);

	Adresse_IP1 : constant T_Adresse_IP := Transfo_Ad_IP (128, 255, 86, 70);
	Adresse_IP2 : constant T_Adresse_IP := Transfo_Ad_IP (128, 255, 86, 75);
	Adresse_IP3 : constant T_Adresse_IP := Transfo_Ad_IP (70, 87, 1, 70);
	Adresse_IP4 : constant T_Adresse_IP := Transfo_Ad_IP (210, 60, 86, 70);

	M1 : constant T_Adresse_IP := Transfo_Ad_IP (255, 255, 255, 255);
	M2 : constant T_Adresse_IP := Transfo_Ad_IP (255, 255, 255, 255);
	M3 : constant T_Adresse_IP := Transfo_Ad_IP (255, 255, 255, 0);
	M4 : constant T_Adresse_IP := Transfo_Ad_IP (0, 0, 0, 0);

	Ad_max1 : constant T_Adresse_IP := Transfo_Ad_IP (255, 255, 255, 255);
	Ad_max2 : constant T_Adresse_IP := Transfo_Ad_IP (255, 255, 255, 255);
	Ad_max3 : constant T_Adresse_IP := Transfo_Ad_IP (255, 255, 255, 0);

	package Testeur_Adresse_IP is new Testeur(
		Unbounded_String, T_Adresse_IP, POIDS_FORT_ICI,
		To_Unbounded_String("un"), To_Unbounded_String("deux"), To_Unbounded_String("trois"), To_Unbounded_String("quatre"),
		Ad_IP1, Ad_IP2, Ad_IP3, Ad_IP4,
		M1, M2, M3, M4,
		Ad_max1, Ad_max2, Ad_max3,
		Adresse_IP1, Adresse_IP2, Adresse_IP3, Adresse_IP4, 
		LISTES_Adresse_IP
	);
	use Testeur_Adresse_IP;


   	function Et(Adresse_IP : in T_Adresse_IP; Masque : in T_Adresse_IP) return T_Adresse_IP is
   	begin
      	return Adresse_IP and Masque;
   	end Et;


   	function Produit(Destination : in T_Adresse_IP; bit : in Integer) return T_Adresse_IP is
   	begin
      	return Destination * (2 ** bit);
   	end Produit;


   	procedure Association_Int(Masque : in out T_Adresse_IP; Int : in Integer) is
   	begin
      	Masque := Masque + 2**Int;
   	end Association_Int;

   	procedure Rien(Masque : in out T_Adresse_IP) is
   	begin
		Masque := 0;
   	end Rien;


	function inf(Masque_nouv : in T_Adresse_IP; Masque : in T_Adresse_IP) return Boolean is 
	begin
		return Masque_nouv <= Masque;
	end inf;

	procedure Rien_int(Int : in out Unbounded_String) is
	begin
		Int := To_Unbounded_String("");
	end Rien_int;

	procedure Afficher_Ad_IP(M1 : in T_Adresse_IP) is
	begin
		Put (Natural ((M1 / UN_OCTET ** 3) mod UN_OCTET), 1); 
		Put (".");
		Put (Natural ((M1 / UN_OCTET ** 2) mod UN_OCTET), 1); 
		Put (".");
		Put (Natural ((M1 / UN_OCTET ** 1) mod UN_OCTET), 1); 
		Put (".");
		Put (Natural  (M1 mod UN_OCTET), 1);
	end Afficher_Ad_IP;

	procedure Tester_Tout is new Tester_Tout_Gen (
		Et_Gen => Et,
		Produit_Gen => Produit,
		Association_int_Gen => Association_Int,
		Rien_Gen => Rien,
		inf_Gen => inf,
		Rien_K => Rien_int,
		Afficher_Ad_IP_Gen => Afficher_Ad_IP
	);



begin
	Tester_Tout;

	New_Line;
	Put_Line ("Tous les tests ont réussi.");
end Tester_LISTES;
