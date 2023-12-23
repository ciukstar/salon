
[En français](https://github.com/ciukstar/salon/blob/master/README.fr.md)  

[În română](https://github.com/ciukstar/salon/blob/master/README.ro.md)  

[На русском](https://github.com/ciukstar/salon/blob/master/README.ru.md)

# Salon

The ["Salon"](https://salon-w3cpovaqka-de.a.run.app) application provides the opportunity to publish and advertise the services of beauty salon owners on the web.

## Overview

The services to be advertised are defined and published in section ["Services"](https://salon-w3cpovaqka-de.a.run.app/admin/services) of group "Data". Optionally, services can be defined as a hierarchy of groups and subservices.

Each service can have multiple offers with corresponding prices. Offers are defined for each service in the “Services” section in the “Data” group.

Once the service and its offerings are defined, the service can be published. The service and offers will be displayed to the customer in the section ["Services"](https://salon-w3cpovaqka-de.a.run.app/services) and available for booking in the section ["Book appointment"](https://salon-w3cpovaqka-de.a.run.app/book).

# Basic Entities

## Business

Detailed information about the business can be provided in the section ["Business"](https://salon-w3cpovaqka-de.a.run.app/admin/business) of the group "Data" in the main menu.

Additionally, from the tab ["Schedule"](https://salon-w3cpovaqka-de.a.run.app/admin/business/1/hours) in the section ["Business"](https://salon-w3cpovaqka-de.a.run.app/admin/business), the organization's work schedule for each day can be added.

The "About Us" page can be customized from the ["About Us"](https://salon-w3cpovaqka-de.a.run.app/admin/about/business/1) tab in the menu ["Business"](https://salon-w3cpovaqka-de.a.run.app/admin/business) in the group "Data".

The "Contacts" page can be customized from the ["Contact Us"](https://salon-w3cpovaqka-de.a.run.app/admin/contact/business/1) tab in the menu ["Business"](https://salon-w3cpovaqka-de.a.run.app/admin/business) in the group "Data".

The [“Home”](https://salon-w3cpovaqka-de.a.run.app) page is customizable from the [“Brand”](https://salon-w3cpovaqka-de.a.run.app/admin/business/1/brand) tab of the [“Business”](https://salon-w3cpovaqka-de.a.run.app/admin/business) menu in the “Data” group.

Currently, the app only supports one business. Multi-business support is planned for future versions of the application.

## User

Application users, including clients, employees and administrators, can be registered through the ["Registration"](https://salon-w3cpovaqka-de.a.run.app/account) form.

Users can also be managed by administrators in the section [“Users”](https://salon-w3cpovaqka-de.a.run.app/admin/users) of the group “Data”.

A registered user can have several roles at the same time.

An administrator can assign the role "Administrator" to any other registered user. The role “Administrator” is required in order to have access to any section of the group “Data”. That is, to manage the configuration and data of the application.

An administrator can assign the role "Analyst" to any other registered user. The role “Analyst” is required in order to have access to any section of the group “Analytics”.

Any user who is also registered as a staff member has the role "Employee". A user with the role "Employee" has access to the service request queue.

All users can become customers by simply registering and using the services offered.

## Staff
...

## Service
...

## Offer
...

## Invoice

An invoice can be created manually in the [“Invoices”](https://salon-w3cpovaqka-de.a.run.app/admin/billing/invoices) section by providing its details and the constituent items.

After creating a new invoice and adding its elements, the invoice can be sent to the customer to a specified email address.

Additionally, the invoice can be downloaded in PDF or HTML format and sent to the customer by other means of communication.

# Integration with external APIs

* Payment gateway: [Stripe](https://stripe.com/)
  ```
  ENV: YESOD_STRIPE_PK, YESOD_STRIPE_SK
  ```
  To simulate a successful payment, use test cards from the [following list](https://stripe.com/docs/testing?testing-method=card-numbers#cards).

  To simulate payments that the issuer declines, use test cards from the [following list](https://stripe.com/docs/testing?testing-method=card-numbers#declined-payments).

* Online maps: [Mapbox](https://www.mapbox.com/)
  ```
  ENV: YESOD_MAPBOX_PK
  ```
  
* Email: [Gmail API](https://developers.google.com/gmail/api/guides)
  ```
  ENV: YESOD_GOOGLE_CLIENT_ID, YESOD_GOOGLE_CLIENT_SECRET
  ```

## ERD Diagram

![Entity Relationship Diagram](static/img/Salon-ERD.svg)

## State transition diagram for booking

![State transition diagram for booking](static/img/Booking-State-Diagram.svg)

## State transition diagram for appointment

![State transition diagram for appointment](static/img/Appointment-State-Transition.svg)

## Demo

[Click here to see demo](https://salon-w3cpovaqka-de.a.run.app)

_* Click on the [![Demography icon](static/img/demography_FILL0_wght400_GRAD0_opsz24.svg)](https://salon-w3cpovaqka-de.a.run.app/auth/login) icon to get a list of demo accounts_
