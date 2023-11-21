
[In english](https://github.com/ciukstar/salon/blob/master/README.md)  

[În română](https://github.com/ciukstar/salon/blob/master/README.ro.md)  

[На русском](https://github.com/ciukstar/salon/blob/master/README.ru.md)

# Salon

L'application [« Salon »](https://salonfr-w3cpovaqka-de.a.run.app) offre la possibilité de publier et d'annoncer les services des propriétaires de salons de beauté sur le Web.

## Aperçu

Les services à annoncer sont définis et publiés dans la section [« Services »](https://salonfr-w3cpovaqka-de.a.run.app/admin/services) du groupe « Données ». Si nécessaire, les services peuvent être définis comme une hiérarchie de groupes et de sous-services.

Chaque service peut avoir plusieurs offres avec le prix correspondant. Les offres sont définies pour chaque service dans la rubrique « Services » du groupe « Données ».

Une fois le service et ses offres définis, le service peut être publié. La prestation et les offres seront présentées au client dans la rubrique [« Services »](https://salonfr-w3cpovaqka-de.a.run.app/services) et disponibles à la réservation dans la rubrique [« Prenez rendez-vous »](https://salonfr-w3cpovaqka-de.a.run.app/book).

# Entités de base

## Entreprise

Des informations détaillées sur l'entreprise peuvent être fournies dans la section [« Entreprise »](https://salonfr-w3cpovaqka-de.a.run.app/admin/business) du groupe « Données » du menu principal.

De plus, à partir de l'onglet [« Horaire »](https://salonfr-w3cpovaqka-de.a.run.app/admin/business/1/hours) de la section [« Entreprise »](https://salonfr-w3cpovaqka-de.a.run.app/admin/business), l'horaire de travail de l'organisation pour chaque jour peut être ajouté.

La page « À propos de nous » est personnalisable depuis l'onglet [« À propos de nous »](https://salonfr-w3cpovaqka-de.a.run.app/admin/about/business/1) du menu [« Entreprise »](https://salonfr-w3cpovaqka-de.a.run.app/admin/business) du groupe « Données ».

La page « Contacts » est personnalisable depuis l'onglet [« Contactez-nous »](https://salonfr-w3cpovaqka-de.a.run.app/admin/contact/business/1) du menu [« Entreprise »](https://salonfr-w3cpovaqka-de.a.run.app/admin/business) du groupe « Données ».

La page [« Accueil »](https://salon-w3cpovaqka-de.a.run.app) est personnalisable depuis l'onglet [« Marque »](https://salon-w3cpovaqka-de.a.run.app/admin/business/1/brand) du menu [« Entreprise »](https://salon-w3cpovaqka-de.a.run.app/admin/business) du groupe « Données ».

Actuellement, l'application ne prend en charge qu'une seule entreprise. Un support multi-métiers est prévu pour les futures versions de l'application.

## Utilisateur

Les utilisateurs de l'application, y compris les clients, les employés et les administrateurs, peuvent être enregistrés via le formulaire [« Inscription »](https://salonfr-w3cpovaqka-de.a.run.app/account).

De plus, les utilisateurs peuvent être gérés par les administrateurs dans la section [« Utilisateurs »](https://salonfr-w3cpovaqka-de.a.run.app/admin/users) du groupe « Données ».

Un utilisateur enregistré peut avoir plusieurs rôles en même temps.

Un administrateur peut attribuer le rôle « Administrateur » à tout autre utilisateur enregistré. Le rôle « Administrateur » est requis pour avoir accès à n'importe quelle section du groupe « Données ». C'est-à-dire gérer la configuration et les données de l'application.

Un administrateur peut attribuer le rôle « Analyste » à tout autre utilisateur enregistré. Le rôle « Analyste » est requis pour avoir accès à n'importe quelle section du groupe « Analyse ». Autrement dit, pour générer des rapports analytiques.

Tout utilisateur également enregistré en tant que membre du personnel a le rôle "Employé". Un utilisateur avec le rôle « Employé » a accès à la file d'attente des demandes de service.

Tous les utilisateurs peuvent devenir clients par simple inscription et utilisation des services proposés.

## Diagramme ERD

![Diagramme entité-relation](static/img/Salon-ERD.svg)

## Schéma de transition d'état pour la réservation

![Diagramme de transition d’état pour la réservation](static/img/Booking-State-Diagram.svg)

## Diagramme de transition d'état de rendez-vous

![Schéma de transition d'état pour rendez-vous](static/img/Appointment-State-Transition.svg)

## Démo

[Cliquez ici pour voir la démo](https://salonfr-w3cpovaqka-de.a.run.app)

_* Cliquez sur l'icône [<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 -960 960 960" width="24"><path d="M720-240q25 0 42.5-17.5T780-300q0-25-17.5-42.5T720-360q-25 0-42.5 17.5T660-300q0 25 17.5 42.5T720-240Zm0 120q30 0 56-14t43-39q-23-14-48-20.5t-51-6.5q-26 0-51 6.5T621-173q17 25 43 39t56 14Zm-520 0q-33 0-56.5-23.5T120-200v-560q0-33 23.5-56.5T200-840h560q33 0 56.5 23.5T840-760v268q-19-9-39-15.5t-41-9.5v-243H200v560h242q3 22 9.5 42t15.5 38H200Zm0-120v40-560 243-3 280Zm80-40h163q3-21 9.5-41t14.5-39H280v80Zm0-160h244q32-30 71.5-50t84.5-27v-3H280v80Zm0-160h400v-80H280v80ZM720-40q-83 0-141.5-58.5T520-240q0-83 58.5-141.5T720-440q83 0 141.5 58.5T920-240q0 83-58.5 141.5T720-40Z"/></svg>](https://salonfr-w3cpovaqka-de.a.run.app/auth/login) pour obtenir une liste des comptes d'utilisateurs de démonstration_
