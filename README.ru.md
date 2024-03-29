
[In english](https://github.com/ciukstar/salon/blob/master/README.md)  

[En français](https://github.com/ciukstar/salon/blob/master/README.fr.md)  

[În română](https://github.com/ciukstar/salon/blob/master/README.ro.md)

# Салон

Приложение [«Салон»](https://salonru-w3cpovaqka-de.a.run.app) предоставляет возможность публиковать и рекламировать услуги владельцев салонов красоты в сети.

## Обзор

Рекламируемые услуги определены и опубликованы в разделе [«Услуги»](https://salonru-w3cpovaqka-de.a.run.app/admin/services) группы «Данные». При желании услуги можно определить как иерархию групп и подуслуг.

Каждая услуга может иметь несколько предложений с соответствующими ценами. Предложения определены для каждой услуги в разделе «Услуги» в группе «Данные».

После определения службы и ее предложений ее можно опубликовать. Услуга и предложения будут отображены клиенту в разделе [«Услуги»](https://salonru-w3cpovaqka-de.a.run.app/services) и доступны для бронирования в разделе [«Записаться на приём»](https://salonru-w3cpovaqka-de.a.run.app/book).

# Основные сущности

## Организация

Подробную информацию об организации можно предоставить в разделе [«Организация»](https://salonru-w3cpovaqka-de.a.run.app/admin/business) группы «Данные» главного меню.

Дополнительно в разделе [«Организация»](https://salonru-w3cpovaqka-de.a.run.app/admin/business) на вкладке [«График»](https://salonru-w3cpovaqka-de.a.run.app/admin/business/1/hours) можно добавить график работы организации на каждый день.

Страницу «О нас» можно настроить на вкладке [«О нас»](https://salonru-w3cpovaqka-de.a.run.app/admin/about/business/1) в меню [«Бизнес»](https://salonru-w3cpovaqka-de.a.run.app/admin/business) в группе «Данные».

Страницу «Контакты» можно настроить на вкладке [«Контакты»](https://salonru-w3cpovaqka-de.a.run.app/admin/contact/business/1) в меню [«Бизнес»](https://salonru-w3cpovaqka-de.a.run.app/admin/business) в группе «Данные».

[«Главная»](https://salon-w3cpovaqka-de.a.run.app) страница настраивается на вкладке [«Бренд»](https://salon-w3cpovaqka-de.a.run.app/admin/business/1/brand) меню [«Организация»](https://salon-w3cpovaqka-de.a.run.app/admin/business) в группе «Данные».

В настоящее время приложение поддерживает только один бизнес. Поддержка нескольких организаций планируется в будущих версиях приложения.

## Пользователь

Пользователи приложения, включая клиентов, сотрудников и администраторов, могут быть зарегистрированы через форму [«Регистрация»](https://salonru-w3cpovaqka-de.a.run.app/account).

Также управлять пользователями администраторы могут в разделе [«Пользователи»](https://salonru-w3cpovaqka-de.a.run.app/admin/users) группы «Данные».

Зарегистрированный пользователь может иметь несколько ролей одновременно.

Администратор может назначить роль «Администратор» любому другому зарегистрированному пользователю. Роль «Администратор» необходима для того, чтобы иметь доступ к любому разделу группы «Данные». То есть управлять настройкой и данными приложения.

Администратор может назначить роль «Аналитик» любому другому зарегистрированному пользователю. Роль «Аналитик» необходима для того, чтобы иметь доступ к любому разделу группы «Аналитика». То есть формировать аналитические отчеты.

Любой пользователь, который также зарегистрирован как сотрудник, имеет роль «Сотрудник». Пользователь с ролью «Сотрудник» имеет доступ к очереди запросов на обслуживание.

Все пользователи могут стать клиентами, просто зарегистрировавшись и воспользовавшись предлагаемыми услугами.

## Персонал
...

## Услуга
...

## Предложение
...

## Счет-фактура

Счет-фактуру можно создать вручную в разделе [«Счета-фактуры»](https://salonru-w3cpovaqka-de.a.run.app/admin/billing/invoices), указав его реквизиты и составляющие его позиции.

После создания нового счета-фактуры и добавления его элементов счет можно отправить покупателю на указанный адрес электронной почты.

Кроме того, счет-фактуру можно скачать в формате PDF или HTML и отправить клиенту другими средствами связи.

# Интеграция с внешними API
* Платежный шлюз: [Stripe](https://stripe.com/)
  ```
  ENV: YESOD_STRIPE_PK, YESOD_STRIPE_SK
  ```
  
  Для имитации успешного платежа используйте тестовые карты из [следующего списка](https://stripe.com/docs/testing?testing-method=card-numbers#cards).

  Для имитации платежей, от которых эмитент отклоняет, используйте тестовые карты из [следующего списка](https://stripe.com/docs/testing?testing-method=card-numbers#declined-payments).

* Онлайн карты: [Mapbox](https://www.mapbox.com/)
  ```
  ENV: YESOD_MAPBOX_PK
  ```

* Электронная почта: [Gmail API](https://developers.google.com/gmail/api/guides)
  ```
  ENV: YESOD_GOOGLE_CLIENT_ID, YESOD_GOOGLE_CLIENT_SECRET
  ```

## Поисковая оптимизация

* [Яндекс Вебмастер](https://webmaster.yandex.ru/welcome)

  ```$YESOD_YANDEX_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```

## ER-диаграмма

![Диаграмма отношений сущностей](static/img/Salon-ERD.svg)

## Диаграмма перехода состояний для бронирования

![Схема перехода состояний для бронирования](static/img/Booking-State-Diagram.svg)

## Схема перехода состояний для записи на приём

![Диаграмма перехода состояний для записи на приём](static/img/Appointment-State-Transition.svg)

## Демо

[Нажмите здесь, чтобы увидеть демо](https://salonru-w3cpovaqka-de.a.run.app)

_* Нажмите на значок [![Demography icon](static/img/demography_FILL0_wght400_GRAD0_opsz24.svg)](https://salonru-w3cpovaqka-de.a.run.app/auth/login), чтобы получить список демонстрационных учетных записей пользователей_
