const header = document.querySelector("[data-header]");
const menu = document.querySelector("[data-menu]");
const menuToggle = document.querySelector("[data-menu-toggle]");
const cartCount = document.querySelector("[data-cart-count]");
const cartView = document.querySelector("[data-cart-view]");
const productDetail = document.querySelector("[data-product-detail]");
const contactForm = document.querySelector("[data-contact-form]");
const cookieBanner = document.querySelector("[data-cookie-banner]");

const LANGUAGE_KEY = "okiyama-language";
const textNodeOriginals = new WeakMap();
const attributeOriginals = new WeakMap();
let originalTitle = document.title;
let currentLanguage = readLanguagePreference();

const translations = {
  en: {
    "Okiyama ofrece productos orientados al cuidado y bienestar diario, con información clara, atención cercana y compra en línea.": "Okiyama offers products focused on daily care and wellness, with clear information, close support and online shopping.",
    "Sobre nosotros :: Okiyama.com": "About us :: Okiyama.com",
    "Tienda Online :: Okiyama.com": "Online Store :: Okiyama.com",
    "Aviso legal y privacidad :: Okiyama.com": "Legal notice and privacy :: Okiyama.com",
    "Cesta de la compra :: Okiyama.com": "Shopping cart :: Okiyama.com",
    "Cesta de la compra": "Shopping cart",
    "Turmgel 50 tabletas :: Okiyama.com": "Turmgel 50 tablets :: Okiyama.com",
    "Turmgel 100 tabletas :: Okiyama.com": "Turmgel 100 tablets :: Okiyama.com",
    "Turmgel 200 tabletas :: Okiyama.com": "Turmgel 200 tablets :: Okiyama.com",
    "Producto Turmgel :: Okiyama.com": "Turmgel product :: Okiyama.com",
    "Tienda online de Okiyama con presentaciones Turmgel, información de compra y recomendaciones de uso responsable.": "Okiyama online store with Turmgel presentations, purchase information and responsible-use recommendations.",
    "Aviso legal, términos, condiciones y política de privacidad de Okiyama SA de CV.": "Legal notice, terms, conditions and privacy policy of Okiyama SA de CV.",
    "Cesta de compra de Okiyama.": "Okiyama shopping cart.",
    "Detalle de producto Turmgel de Okiyama.": "Okiyama Turmgel product detail.",
    "Turmgel 50 tabletas, suplemento alimenticio de cúrcuma en hidrogel.": "Turmgel 50 tablets, hydrogel turmeric dietary supplement.",
    "Turmgel 100 tabletas, suplemento alimenticio de cúrcuma en hidrogel.": "Turmgel 100 tablets, hydrogel turmeric dietary supplement.",
    "Turmgel 200 tabletas, suplemento alimenticio de cúrcuma en hidrogel.": "Turmgel 200 tablets, hydrogel turmeric dietary supplement.",
    "Saltar al contenido": "Skip to content",
    "Okiyama inicio": "Okiyama home",
    "Abrir menú": "Open menu",
    "Cerrar menú": "Close menu",
    "Menú principal": "Main menu",
    "Ir a la cesta": "Go to cart",
    "Enlaces secundarios": "Secondary links",
    "Inicio": "Home",
    "Sobre nosotros": "About us",
    "Tienda Online": "Online Store",
    "Aviso legal y privacidad": "Legal notice and privacy",
    "Contacto": "Contact",
    "Cesta": "Cart",
    "Revisa tus productos antes de continuar.": "Review your products before continuing.",
    "Cookies": "Cookies",
    "Idiomas": "Languages",
    "Español": "Spanish",
    "English": "English",
    "Av. José María Izazaga 50 Oficina 103 A, Colonia Centro, México.": "Av. José María Izazaga 50, Office 103 A, Centro, Mexico.",
    "Utilizamos cookies para permitir un correcto funcionamiento y seguro en nuestra página web, y para ofrecer la mejor experiencia posible.": "We use cookies to keep our website working properly and securely, and to offer the best possible experience.",
    "Aceptar todo": "Accept all",
    "Aceptar solo lo necesario": "Accept necessary only",

    "Cuidado y bienestar": "Care and wellness",
    "Productos seleccionados para acompañar rutinas de cuidado diario, con información responsable y atención cercana.": "Selected products to support daily care routines, with responsible information and close support.",
    "Nuestro enfoque": "Our focus",
    "Claridad, atención y compra confiable": "Clarity, support and reliable purchasing",
    "Información clara": "Clear information",
    "Presentamos cada producto con datos útiles para comprar con mayor confianza.": "We present each product with useful details so customers can buy with greater confidence.",
    "Atención cercana": "Close support",
    "Mantenemos canales visibles para resolver dudas antes y después de la compra.": "We keep visible channels to answer questions before and after purchase.",
    "Uso responsable": "Responsible use",
    "Promovemos la revisión de indicaciones, advertencias y consulta profesional cuando corresponda.": "We encourage reviewing directions, warnings and professional advice when appropriate.",
    "Compra ordenada": "Organized purchase",
    "La tienda concentra presentaciones, precios y cesta para mantener el flujo simple.": "The store brings presentations, prices and the cart together to keep the flow simple.",
    "Confianza": "Trust",
    "Una experiencia pensada para salud y bienestar": "An experience designed for health and wellness",
    "En productos de cuidado personal, la confianza se construye con información verificable, políticas claras y contacto humano.": "In personal care products, trust is built with verifiable information, clear policies and human contact.",
    "Producto visible": "Visible product",
    "Usamos fotografías reales de las presentaciones para que el cliente sepa qué está comprando.": "We use real photos of the presentations so customers know what they are buying.",
    "Advertencias visibles": "Visible warnings",
    "Las páginas de producto incluyen notas de uso responsable y recomendaciones antes de comprar.": "Product pages include responsible-use notes and recommendations before purchase.",
    "Políticas accesibles": "Accessible policies",
    "El aviso legal, privacidad, contacto y condiciones de compra permanecen disponibles en todo el sitio.": "Legal notice, privacy, contact and purchase conditions remain available throughout the site.",
    "Nuevo Producto": "New Product",
    "Turmgel - Suplemento de Cúrcuma": "Turmgel - Turmeric Supplement",
    "Turmgel es el producto destacado de Okiyama, creado para acompañar rutinas de bienestar con una presentación práctica de cúrcuma en hidrogel.": "Turmgel is Okiyama's featured product, created to support wellness routines with a practical hydrogel turmeric presentation.",
    "Para revisar presentaciones, precios y detalles de compra, visita la tienda online.": "To review presentations, prices and purchase details, visit the online store.",
    "Ver presentaciones": "View presentations",
    "Contáctanos por WhatsApp": "Contact us on WhatsApp",
    "Presentación de Turmgel": "Turmgel presentation",

    "Historia, equipo y contacto en un solo lugar.": "History, team and contact in one place.",
    "Una empresa con presencia en el cuidado de la salud": "A company with a presence in health care",
    "Okiyama SA de CV es una spin-off de SACHET SA de CV, grupo empresarial que ingresó al mercado mexicano en 2004 para distribuir productos de consumo.": "Okiyama SA de CV is a spin-off of SACHET SA de CV, a business group that entered the Mexican market in 2004 to distribute consumer products.",
    "Okiyama fue fundada el 17 de junio de 2008, y desde 2022 enfoca su actividad en productos orientados al cuidado y bienestar diario de la población mexicana.": "Okiyama was founded on June 17, 2008, and since 2022 has focused its activity on products aimed at the daily care and wellness of the Mexican population.",
    "La empresa trabaja con miras a tener una importante presencia a nivel nacional e internacional.": "The company works toward building a strong national and international presence.",
    "Datos de Okiyama": "Okiyama facts",
    "Ingreso del grupo empresarial al mercado mexicano": "Business group entered the Mexican market",
    "Fundación de Okiyama SA de CV": "Foundation of Okiyama SA de CV",
    "Enfoque en productos para el cuidado de la salud": "Focus on health care products",
    "Nuestro equipo": "Our team",
    "Desarrollo y Compromiso": "Development and Commitment",
    "Nuestro equipo se caracteriza por tener personas comprometidas con su desarrollo profesional, con el crecimiento de la empresa y con una atención clara para clientes, aliados y profesionales de la salud.": "Our team is made up of people committed to their professional development, to the company's growth and to clear support for customers, partners and health professionals.",
    "Material de laboratorio farmacéutico": "Pharmaceutical laboratory materials",
    "Horario de oficina": "Office hours",
    "Lunes — Viernes 9am — 6pm": "Monday — Friday 9am — 6pm",

    "Tienda Online Okiyama": "Okiyama Online Store",
    "Presentaciones Turmgel con información clara, precio visible y compra desde la cesta.": "Turmgel presentations with clear information, visible prices and cart-based purchasing.",
    "Ventajas de compra": "Purchase benefits",
    "Envío gratis en productos Turmgel": "Free shipping on Turmgel products",
    "Pago preparado con Openpay": "Payment prepared with Openpay",
    "Atención por WhatsApp y correo": "Support by WhatsApp and email",
    "Productos": "Products",
    "Catálogo Turmgel": "Turmgel Catalog",
    "Selecciona una presentación, revisa el detalle del producto y añade al carrito.": "Select a presentation, review the product details and add it to the cart.",
    "Turmgel 50 tabletas": "Turmgel 50 tablets",
    "Turmgel 100 tabletas": "Turmgel 100 tablets",
    "Turmgel 200 tabletas": "Turmgel 200 tablets",
    "Empaque de 5 cajas de 10 tabletas de hidrogel.": "Pack of 5 boxes of 10 hydrogel tablets.",
    "Empaque de 10 cajas de 10 tabletas de hidrogel.": "Pack of 10 boxes of 10 hydrogel tablets.",
    "20 cajas con 10 tabletas cada una.": "20 boxes with 10 tablets each.",
    "Añadir a la cesta": "Add to cart",
    "Añadido a la cesta": "Added to cart",
    "Ver detalle": "View details",
    "Ver detalle de Turmgel 50 tabletas": "View details for Turmgel 50 tablets",
    "Ver detalle de Turmgel 100 tabletas": "View details for Turmgel 100 tablets",
    "Ver detalle de Turmgel 200 tabletas": "View details for Turmgel 200 tablets",
    "Los productos publicados se presentan con fines informativos y de compra. Revisa las recomendaciones de uso responsable en cada página de producto.": "Published products are presented for informational and purchasing purposes. Review the responsible-use recommendations on each product page.",
    "Antes de comprar": "Before buying",
    "Información útil para decidir con tranquilidad": "Useful information to decide with confidence",
    "Consulta profesional": "Professional advice",
    "Si estás bajo tratamiento, tienes una condición médica o compras para un paciente, consulta a un profesional de la salud.": "If you are under treatment, have a medical condition or are buying for a patient, consult a health professional.",
    "Revisa la presentación": "Review the presentation",
    "Cada ficha muestra cantidad, precio y contenido del empaque para evitar confusiones al comprar.": "Each product page shows quantity, price and package contents to avoid confusion when buying.",
    "Conserva tu pedido": "Keep your order details",
    "Al finalizar la compra, guarda la confirmación y comunícate con Okiyama ante cualquier duda de entrega.": "After completing the purchase, keep the confirmation and contact Okiyama with any delivery questions.",

    "Suplemento alimenticio": "Dietary supplement",
    "Presentación de cúrcuma en hidrogel para acompañar rutinas de bienestar y cuidado diario.": "Hydrogel turmeric presentation to support wellness and daily care routines.",
    "Presentación de inicio": "Starter presentation",
    "Presentación intermedia": "Mid-size presentation",
    "Presentación mayor": "Larger presentation",
    "Contenido": "Contents",
    "5 cajas de 10 tabletas": "5 boxes of 10 tablets",
    "10 cajas de 10 tabletas": "10 boxes of 10 tablets",
    "20 cajas de 10 tabletas": "20 boxes of 10 tablets",
    "50 tabletas": "50 tablets",
    "100 tabletas": "100 tablets",
    "200 tabletas": "200 tablets",
    "Formato": "Format",
    "Tabletas de hidrogel": "Hydrogel tablets",
    "Envío": "Shipping",
    "Gratis en productos Turmgel": "Free on Turmgel products",
    "Revisa el empaque, indicaciones y recomendaciones profesionales antes de consumirlo.": "Review the package, directions and professional recommendations before consuming it.",
    "Ver cesta": "View cart",
    "Volver a productos": "Back to products",
    "Producto no encontrado": "Product not found",
    "No encontramos esta presentación. Vuelve al catálogo para elegir un producto disponible.": "We could not find this presentation. Return to the catalog to choose an available product.",
    "Información del producto": "Product information",
    "Compra con datos claros": "Purchase with clear details",
    "Composición": "Composition",
    "Cúrcuma en presentación de hidrogel. Verifica siempre la información impresa en el empaque recibido.": "Turmeric in hydrogel presentation. Always verify the information printed on the package received.",
    "Modo de uso": "Directions for use",
    "Consumir únicamente conforme a la recomendación profesional o las instrucciones del producto.": "Consume only according to professional recommendation or product instructions.",
    "Conservación": "Storage",
    "Mantener en un lugar fresco, seco, cerrado y fuera del alcance de menores.": "Keep in a cool, dry, closed place and out of the reach of minors.",
    "No sustituye una dieta equilibrada ni la atención de un profesional de la salud.": "It does not replace a balanced diet or care from a health professional.",
    "No se presenta como medicamento ni como tratamiento para enfermedades.": "It is not presented as a medicine or as a treatment for diseases.",
    "Consulta antes de usarlo si estás bajo tratamiento, embarazo, lactancia o tienes una condición médica.": "Consult before using it if you are under treatment, pregnant, breastfeeding or have a medical condition.",
    "Preguntas frecuentes": "Frequently asked questions",
    "Antes de añadir a la cesta": "Before adding to cart",
    "¿Esta presentación para quién conviene?": "Who is this presentation suitable for?",
    "Es una opción práctica para probar el producto o para compras de menor volumen.": "It is a practical option for trying the product or for lower-volume purchases.",
    "Es una alternativa para quienes buscan una cantidad media o compra recurrente.": "It is an alternative for those looking for a medium quantity or recurring purchase.",
    "Es una opción para compras de mayor volumen o abastecimiento prolongado.": "It is an option for higher-volume purchases or longer-term supply.",
    "¿Puedo recibir orientación?": "Can I receive guidance?",
    "Sí. Puedes contactar a Okiyama por WhatsApp o correo antes de realizar tu compra.": "Yes. You can contact Okiyama by WhatsApp or email before making your purchase.",
    "¿Cómo continúa la compra?": "How does the purchase continue?",
    "Añade el producto a la cesta, revisa cantidades y completa tus datos para continuar con el pago.": "Add the product to the cart, review quantities and complete your details to continue with payment.",
    "Empaque Turmgel 50 tabletas": "Turmgel 50-tablet package",
    "Empaque Turmgel 100 tabletas": "Turmgel 100-tablet package",
    "Empaque Turmgel 200 tabletas": "Turmgel 200-tablet package",

    "Legal": "Legal",
    "Términos, condiciones y protección de datos personales de Okiyama.": "Terms, conditions and personal data protection at Okiyama.",
    "Términos y condiciones": "Terms and conditions",
    "Okiyama SA de CV gestiona este sitio web. Al visitar el sitio o realizar una compra, el usuario acepta los términos, condiciones, políticas y avisos publicados en esta página.": "Okiyama SA de CV manages this website. By visiting the site or making a purchase, the user accepts the terms, conditions, policies and notices published on this page.",
    "Uso de la tienda": "Store use",
    "Los productos y servicios pueden estar disponibles exclusivamente en línea y sujetos a cantidades limitadas. Las descripciones, precios, disponibilidad y promociones pueden cambiar sin previo aviso.": "Products and services may be available exclusively online and subject to limited quantities. Descriptions, prices, availability and promotions may change without prior notice.",
    "Información de compra y cuenta": "Purchase and account information",
    "El cliente acepta proporcionar información completa y precisa para sus compras, incluyendo datos de contacto, facturación, envío y cualquier dato necesario para completar la transacción.": "The customer agrees to provide complete and accurate information for purchases, including contact, billing, shipping and any data needed to complete the transaction.",
    "Productos y responsabilidad de consumo": "Products and consumption responsibility",
    "La información de los productos publicados en esta página tiene fines informativos y comerciales. No sustituye la orientación de un profesional de la salud, ni debe interpretarse como diagnóstico, tratamiento o prevención de enfermedades.": "Product information published on this page is for informational and commercial purposes. It does not replace guidance from a health professional and should not be interpreted as diagnosis, treatment or disease prevention.",
    "El consumo de los productos vendidos en esta página es responsabilidad de quien los recomienda y de quien los usa. Se recomienda consultar a un profesional de la salud para establecer la dosis o forma de consumo adecuada.": "Consumption of products sold on this page is the responsibility of the person who recommends them and the person who uses them. We recommend consulting a health professional to establish the appropriate dose or method of consumption.",
    "Pagos, entrega y devoluciones": "Payments, delivery and returns",
    "Las transacciones podrán realizarse mediante pasarela de pago. La entrega de productos se realizará dentro de los plazos indicados durante la compra.": "Transactions may be made through a payment gateway. Product delivery will take place within the time frames indicated during purchase.",
    "Se aceptará devolución cuando la cantidad no corresponda a la solicitada, cuando la fecha de caducidad esté vencida o próxima a vencer antes del consumo correspondiente, o cuando el producto llegue en mal estado o con defectos de fabricación.": "Returns will be accepted when the quantity does not match the request, when the expiration date has passed or is close to expiring before the expected consumption period, or when the product arrives in poor condition or with manufacturing defects.",
    "Errores, omisiones y usos prohibidos": "Errors, omissions and prohibited uses",
    "Okiyama SA de CV puede corregir errores, actualizar información, modificar servicios o cambiar precios sin previo aviso cuando sea necesario para mantener la operación del sitio.": "Okiyama SA de CV may correct errors, update information, modify services or change prices without prior notice when necessary to maintain site operation.",
    "Está prohibido utilizar el sitio con fines ilegales, transmitir código dañino, enviar información falsa, afectar la seguridad del servicio o infringir derechos de terceros.": "It is forbidden to use the site for illegal purposes, transmit harmful code, send false information, affect service security or infringe third-party rights.",
    "Política de privacidad": "Privacy policy",
    "Cuando una persona visita el sitio, pueden recopilarse datos técnicos del dispositivo, como navegador, dirección IP, zona horaria, cookies instaladas y páginas visitadas.": "When a person visits the site, technical device data may be collected, such as browser, IP address, time zone, installed cookies and pages visited.",
    "Cuando el usuario realiza o intenta realizar una compra, pueden solicitarse datos de pedido como nombre, dirección, correo electrónico, teléfono e información necesaria para el procesamiento del pago.": "When the user makes or attempts to make a purchase, order data such as name, address, email, phone number and information needed for payment processing may be requested.",
    "Uso de la información personal": "Use of personal information",
    "La información se utiliza para preparar pedidos, procesar pagos, organizar envíos, emitir confirmaciones, comunicarse con el cliente y detectar posibles riesgos o fraudes.": "Information is used to prepare orders, process payments, organize shipments, issue confirmations, communicate with the customer and detect possible risks or fraud.",
    "Cookies y analítica": "Cookies and analytics",
    "El sitio puede usar cookies esenciales, funcionales, de rendimiento y de marketing. También puede emplear herramientas de analítica para comprender cómo interactúan los clientes con la página.": "The site may use essential, functional, performance and marketing cookies. It may also use analytics tools to understand how customers interact with the page.",
    "Destinatarios y procesadores": "Recipients and processors",
    "Los datos pueden ser tratados por subcontratistas indispensables para la operación del sitio, como plataforma de tienda online, empresas transportistas y servicios de analítica.": "Data may be processed by subcontractors necessary for site operation, such as the online store platform, shipping companies and analytics services.",
    "Derechos del cliente": "Customer rights",
    "Derecho de acceso a los datos personales.": "Right of access to personal data.",
    "Derecho a la rectificación de datos personales.": "Right to rectify personal data.",
    "Derecho a borrar datos personales.": "Right to delete personal data.",
    "Derecho a oponerse al procesamiento.": "Right to object to processing.",
    "Derecho a la portabilidad de datos.": "Right to data portability.",
    "Derecho a retirar el consentimiento.": "Right to withdraw consent.",
    "Derecho a presentar una queja ante la autoridad supervisora.": "Right to file a complaint with the supervisory authority.",
    "Seguridad, menores y contacto": "Security, minors and contact",
    "Okiyama declara tomar precauciones técnicas y organizativas para proteger los datos personales. El sitio no está destinado a personas menores de 18 años.": "Okiyama states that it takes technical and organizational precautions to protect personal data. The site is not intended for people under 18 years old.",
    "Para dudas sobre privacidad o condiciones de uso, se puede escribir a Okiyamamx@gmail.com o acudir a 50 Avenida Izazaga, Ciudad de México, DF, 08060, México.": "For questions about privacy or terms of use, you can write to Okiyamamx@gmail.com or visit 50 Avenida Izazaga, Mexico City, DF, 08060, Mexico.",
    "La política de privacidad publicada por Okiyama indica vigencia desde el 11/12/2023.": "Okiyama's published privacy policy indicates it has been in effect since December 11, 2023.",

    "Tu cesta está vacía.": "Your cart is empty.",
    "¡Cámbialo! Elige algo y vuelve aquí.": "Change that. Choose something and come back here.",
    "Seguir comprando": "Continue shopping",
    "Producto": "Product",
    "Precio": "Price",
    "Cantidad": "Quantity",
    "Eliminar": "Remove",
    "Reducir cantidad de": "Decrease quantity of",
    "Aumentar cantidad de": "Increase quantity of",
    "con IVA": "incl. VAT",
    "Pago seguro con Openpay": "Secure payment with Openpay",
    "Completa tus datos para continuar con la compra.": "Complete your details to continue with the purchase.",
    "Total a pagar:": "Total to pay:",
    "Información de compra": "Purchase information",
    "Datos protegidos para completar tu pedido": "Protected data to complete your order",
    "Nombre:": "Name:",
    "Teléfono:": "Phone:",
    "Correo Electrónico:": "Email:",
    "Código de Cupón de Descuento:": "Discount coupon code:",
    "Acepto el": "I accept the",
    "aviso legal y privacidad": "legal notice and privacy policy",
    "Comprar con Openpay": "Buy with Openpay",
    "Tu cesta está vacía. Agrega productos antes de continuar.": "Your cart is empty. Add products before continuing.",
    "Datos listos para pago. Total:": "Details ready for payment. Total:",
    "Falta conectar Openpay para completar la compra.": "Openpay still needs to be connected to complete the purchase.",
    "Gracias. Pronto nos pondremos en contacto.": "Thank you. We will contact you soon.",
    "Cantidad de": "Quantity of"
  }
};

const products = {
  "turmgel-50": {
    id: "turmgel-50",
    title: "Turmgel 50 tabletas",
    image: "assets/turmgel-50.webp",
    width: 1400,
    height: 932,
    price: 950,
    label: "Presentación de inicio",
    contents: "5 cajas de 10 tabletas",
    total: "50 tabletas",
    alt: "Empaque Turmgel 50 tabletas",
    faq: "Es una opción práctica para probar el producto o para compras de menor volumen.",
  },
  "turmgel-100": {
    id: "turmgel-100",
    title: "Turmgel 100 tabletas",
    image: "assets/turmgel-100.webp",
    width: 1300,
    height: 867,
    price: 1500,
    label: "Presentación intermedia",
    contents: "10 cajas de 10 tabletas",
    total: "100 tabletas",
    alt: "Empaque Turmgel 100 tabletas",
    faq: "Es una alternativa para quienes buscan una cantidad media o compra recurrente.",
  },
  "turmgel-200": {
    id: "turmgel-200",
    title: "Turmgel 200 tabletas",
    image: "assets/turmgel-200.webp",
    width: 1300,
    height: 867,
    price: 2900,
    label: "Presentación mayor",
    contents: "20 cajas de 10 tabletas",
    total: "200 tabletas",
    alt: "Empaque Turmgel 200 tabletas",
    faq: "Es una opción para compras de mayor volumen o abastecimiento prolongado.",
  },
};

function readLanguagePreference() {
  try {
    return localStorage.getItem(LANGUAGE_KEY) === "en" ? "en" : "es";
  } catch {
    return "es";
  }
}

function writeLanguagePreference(language) {
  try {
    localStorage.setItem(LANGUAGE_KEY, language);
  } catch {
    // Language persistence can be unavailable in some file:// contexts.
  }
}

function normalizeText(value) {
  return String(value).replace(/\s+/g, " ").trim();
}

function translateFromSpanish(value, language = currentLanguage) {
  if (language === "es") return value;
  const normalized = normalizeText(value);
  return translations.en[normalized] || value;
}

function t(value) {
  return translateFromSpanish(value, currentLanguage);
}

function formatCurrency(value) {
  return new Intl.NumberFormat(currentLanguage === "en" ? "en-US" : "es-MX", {
    style: "currency",
    currency: "MXN",
  }).format(value);
}

function readCart() {
  try {
    return normalizeCartItems(JSON.parse(localStorage.getItem("okiyama-cart") || "[]"));
  } catch {
    return [];
  }
}

function normalizeCartItems(cart) {
  if (!Array.isArray(cart)) return [];
  return cart
    .filter((item) => item && item.id && Number(item.quantity) > 0)
    .map((item) => {
      const product = products[item.id];
      if (!product) return item;
      return {
        ...item,
        name: product.title,
        price: product.price,
        image: product.image,
        quantity: Number(item.quantity),
      };
    });
}

function writeCart(cart) {
  try {
    localStorage.setItem("okiyama-cart", JSON.stringify(cart));
  } catch {
    return;
  }
  updateCartCount();
  renderCart();
}

function updateCartCount() {
  if (!cartCount) return;
  const total = readCart().reduce((sum, item) => sum + item.quantity, 0);
  cartCount.textContent = String(total);
}

function setHeaderState() {
  if (!header) return;
  header.classList.toggle("is-scrolled", window.scrollY > 8);
}

function setMenuButtonLabel(isOpen = false) {
  if (!menuToggle) return;
  menuToggle.setAttribute("aria-label", isOpen ? t("Cerrar menú") : t("Abrir menú"));
}

function syncMenuAccessibility(isOpen = menu?.classList.contains("is-open")) {
  if (!menu || !menuToggle) return;
  const isMobile = window.innerWidth <= 900;
  menuToggle.setAttribute("aria-expanded", String(Boolean(isOpen)));
  setMenuButtonLabel(Boolean(isOpen));
  if (isMobile && !isOpen) {
    menu.setAttribute("aria-hidden", "true");
    menu.querySelectorAll("a").forEach((link) => link.setAttribute("tabindex", "-1"));
    return;
  }
  menu.removeAttribute("aria-hidden");
  menu.querySelectorAll("a").forEach((link) => link.removeAttribute("tabindex"));
}

function closeMenu() {
  if (!menu || !menuToggle) return;
  menu.classList.remove("is-open");
  syncMenuAccessibility(false);
}

function addToCart(product) {
  const cart = readCart();
  const current = cart.find((item) => item.id === product.id);
  if (current) {
    current.quantity += 1;
  } else {
    cart.push({ ...product, quantity: 1 });
  }
  writeCart(cart);
}

function getCurrentProduct() {
  const params = new URLSearchParams(window.location.search);
  return products[params.get("id")] || products["turmgel-50"];
}

function renderProductDetail() {
  if (!productDetail) return;
  const product = getCurrentProduct();
  originalTitle = `${product.title} :: Okiyama.com`;
  document.title = originalTitle;
  productDetail.innerHTML = `
    <section class="section alt">
      <div class="section-shell product-detail">
        <div class="image-panel product-image">
          <img src="${product.image}" width="${product.width}" height="${product.height}" alt="${product.alt}" decoding="async" fetchpriority="high" />
        </div>
        <div class="copy">
          <p class="eyebrow">Suplemento alimenticio</p>
          <h1>${product.title}</h1>
          <p>Presentación de cúrcuma en hidrogel para acompañar rutinas de bienestar y cuidado diario.</p>
          <span class="label-pill">${product.label}</span>
          <dl class="product-specs">
            <div><dt>Contenido</dt><dd>${product.contents}</dd></div>
            <div><dt>Total</dt><dd>${product.total}</dd></div>
            <div><dt>Formato</dt><dd>Tabletas de hidrogel</dd></div>
            <div><dt>Envío</dt><dd>Gratis en productos Turmgel</dd></div>
          </dl>
          <p class="product-note">Revisa el empaque, indicaciones y recomendaciones profesionales antes de consumirlo.</p>
          <div class="price">${formatCurrency(product.price)}</div>
          <div class="hero-actions">
            <button class="button button-primary" type="button" data-add-to-cart data-id="${product.id}" data-name="${product.title}" data-price="${product.price}" data-image="${product.image}">Añadir a la cesta</button>
            <a class="button button-dark" href="cart.html">Ver cesta</a>
            <a class="button button-secondary" href="tienda-online.html">Volver a productos</a>
          </div>
        </div>
      </div>
    </section>

    <section class="section">
      <div class="section-shell">
        <div class="section-heading align-left">
          <p class="eyebrow">Información del producto</p>
          <h2>Compra con datos claros</h2>
        </div>
        <div class="product-info-grid">
          <article class="detail-card">
            <strong>Composición</strong>
            <p>Cúrcuma en presentación de hidrogel. Verifica siempre la información impresa en el empaque recibido.</p>
          </article>
          <article class="detail-card">
            <strong>Modo de uso</strong>
            <p>Consumir únicamente conforme a la recomendación profesional o las instrucciones del producto.</p>
          </article>
          <article class="detail-card">
            <strong>Conservación</strong>
            <p>Mantener en un lugar fresco, seco, cerrado y fuera del alcance de menores.</p>
          </article>
          <article class="warning-box">
            <strong>Uso responsable</strong>
            <ul>
              <li>No sustituye una dieta equilibrada ni la atención de un profesional de la salud.</li>
              <li>No se presenta como medicamento ni como tratamiento para enfermedades.</li>
              <li>Consulta antes de usarlo si estás bajo tratamiento, embarazo, lactancia o tienes una condición médica.</li>
            </ul>
          </article>
        </div>
      </div>
    </section>

    <section class="section alt">
      <div class="section-shell">
        <div class="section-heading align-left">
          <p class="eyebrow">Preguntas frecuentes</p>
          <h2>Antes de añadir a la cesta</h2>
        </div>
        <div class="faq-grid">
          <article class="faq-card">
            <strong>¿Esta presentación para quién conviene?</strong>
            <p>${product.faq}</p>
          </article>
          <article class="faq-card">
            <strong>¿Puedo recibir orientación?</strong>
            <p>Sí. Puedes contactar a Okiyama por WhatsApp o correo antes de realizar tu compra.</p>
          </article>
          <article class="faq-card">
            <strong>¿Cómo continúa la compra?</strong>
            <p>Añade el producto a la cesta, revisa cantidades y completa tus datos para continuar con el pago.</p>
          </article>
        </div>
      </div>
    </section>
  `;
}

function renderCart() {
  if (!cartView) return;
  const cart = readCart();
  if (!cart.length) {
    cartView.innerHTML = `
      <div class="empty-cart">
        <h2>${t("Tu cesta está vacía.")}</h2>
        <p>${t("¡Cámbialo! Elige algo y vuelve aquí.")}</p>
        <a class="button button-primary" href="tienda-online.html">${t("Seguir comprando")}</a>
      </div>
    `;
    return;
  }

  const rows = cart
    .map((item) => {
      const subtotal = item.price * item.quantity;
      const itemName = t(item.name);
      return `
        <tr>
          <td data-label="${t("Producto")}">
            <div class="cart-line">
              <img src="${item.image}" alt="${itemName}" loading="lazy" decoding="async" />
              <strong>${itemName}</strong>
            </div>
          </td>
          <td data-label="${t("Precio")}">${formatCurrency(item.price)}</td>
          <td data-label="${t("Cantidad")}">
            <div class="qty-control" aria-label="${t("Cantidad de")} ${itemName}">
              <button type="button" aria-label="${t("Reducir cantidad de")} ${itemName}" data-cart-decrease="${item.id}">-</button>
              <span>${item.quantity}</span>
              <button type="button" aria-label="${t("Aumentar cantidad de")} ${itemName}" data-cart-increase="${item.id}">+</button>
            </div>
          </td>
          <td data-label="${t("Total")}">${formatCurrency(subtotal)}</td>
          <td><button class="text-button" type="button" aria-label="${t("Eliminar")} ${itemName}" data-cart-remove="${item.id}">${t("Eliminar")}</button></td>
        </tr>
      `;
    })
    .join("");

  const total = cart.reduce((sum, item) => sum + item.price * item.quantity, 0);
  cartView.innerHTML = `
    <table class="cart-table">
      <thead>
        <tr>
          <th>${t("Producto")}</th>
          <th>${t("Precio")}</th>
          <th>${t("Cantidad")}</th>
          <th>${t("Total")}</th>
          <th></th>
        </tr>
      </thead>
      <tbody>${rows}</tbody>
    </table>
    <div class="cart-summary">
      <div>
        <span>${t("con IVA")}</span>
        <strong>${formatCurrency(total)}</strong>
      </div>
      <a class="button button-primary" href="tienda-online.html">${t("Seguir comprando")}</a>
    </div>
    <section class="cart-payment" aria-labelledby="cart-payment-title">
      <div class="cart-payment-copy">
        <h2 id="cart-payment-title">${t("Pago seguro con Openpay")}</h2>
        <p>${t("Completa tus datos para continuar con la compra.")}</p>
        <p class="cart-payment-total">${t("Total a pagar:")} <strong>${formatCurrency(total)}</strong></p>
        <div class="cart-payment-checkpoints" aria-label="${t("Información de compra")}">
          <span>${t("Envío gratis en productos Turmgel")}</span>
          <span>${t("Pago preparado con Openpay")}</span>
          <span>${t("Datos protegidos para completar tu pedido")}</span>
        </div>
      </div>
      <form class="checkout-form cart-payment-form" data-cart-payment-form>
        <label>
          ${t("Nombre:")}
          <input type="text" name="nombre" autocomplete="name" required />
        </label>
        <label>
          ${t("Teléfono:")}
          <input type="tel" name="telefono" autocomplete="tel" required />
        </label>
        <label>
          ${t("Correo Electrónico:")}
          <input type="email" name="email" autocomplete="email" required />
        </label>
        <label>
          ${t("Código de Cupón de Descuento:")}
          <input type="text" name="cupon" autocomplete="off" />
        </label>
        <label class="checkbox-field">
          <input type="checkbox" name="acepta_aviso_legal" value="Sí" required />
          <span>${t("Acepto el")} <a href="aviso-legal.html">${t("aviso legal y privacidad")}</a>.</span>
        </label>
        <button class="button button-primary" type="submit">${t("Comprar con Openpay")}</button>
        <p class="form-note" role="status" data-form-note></p>
      </form>
    </section>
  `;
}

function updateCartItem(id, mode) {
  const cart = readCart();
  const index = cart.findIndex((item) => item.id === id);
  if (index === -1) return;
  if (mode === "increase") cart[index].quantity += 1;
  if (mode === "decrease") cart[index].quantity -= 1;
  if (mode === "remove" || cart[index].quantity <= 0) cart.splice(index, 1);
  writeCart(cart);
}

function getCartTotal(cart) {
  return cart.reduce((sum, item) => sum + item.price * item.quantity, 0);
}

function getOriginalAttribute(element, attribute) {
  let originals = attributeOriginals.get(element);
  if (!originals) {
    originals = new Map();
    attributeOriginals.set(element, originals);
  }
  if (!originals.has(attribute)) originals.set(attribute, element.getAttribute(attribute) || "");
  return originals.get(attribute);
}

function translateAttributes(language) {
  const attributes = ["aria-label", "alt", "content"];
  attributes.forEach((attribute) => {
    document.querySelectorAll(`[${attribute}]`).forEach((element) => {
      if (attribute === "content" && element.getAttribute("name") !== "description") return;
      const original = getOriginalAttribute(element, attribute);
      element.setAttribute(attribute, translateFromSpanish(original, language));
    });
  });

  document.title = translateFromSpanish(originalTitle, language);
  document.querySelectorAll('a[href*="wa.me"]').forEach((link) => {
    const message =
      language === "en"
        ? "Hello, I am interested in Turmgel"
        : "Hola, estoy interesad@ en Turmgel";
    link.href = `https://wa.me/5215528555236?text=${encodeURIComponent(message)}`;
  });
}

function translateStaticText(language) {
  const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, {
    acceptNode(node) {
      const parent = node.parentElement;
      if (!parent || ["SCRIPT", "STYLE", "SVG"].includes(parent.tagName)) {
        return NodeFilter.FILTER_REJECT;
      }
      return normalizeText(node.nodeValue) ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_REJECT;
    },
  });

  const nodes = [];
  while (walker.nextNode()) nodes.push(walker.currentNode);

  nodes.forEach((node) => {
    if (!textNodeOriginals.has(node)) textNodeOriginals.set(node, node.nodeValue);
    const original = textNodeOriginals.get(node);
    if (language === "es") {
      node.nodeValue = original;
      return;
    }

    const translated = translateFromSpanish(original, language);
    if (translated === original) return;
    const leading = original.match(/^\s*/)?.[0] || "";
    const trailing = original.match(/\s*$/)?.[0] || "";
    node.nodeValue = `${leading}${translated}${trailing}`;
  });
}

function renderLanguageControls() {
  document.querySelectorAll(".language-list").forEach((list) => {
    const isHeader = list.classList.contains("header-language");
    list.setAttribute("aria-label", t("Idiomas"));
    list.innerHTML = `
      <strong>${t("Idiomas")}</strong>
      <div class="language-actions">
        <button type="button" data-language-button="es" aria-pressed="${currentLanguage === "es"}">${isHeader ? "ES" : "Español"}</button>
        <button type="button" data-language-button="en" lang="en" aria-pressed="${currentLanguage === "en"}">${isHeader ? "EN" : "English"}</button>
      </div>
    `;
  });
}

function applyLanguage(language) {
  currentLanguage = language === "en" ? "en" : "es";
  document.documentElement.lang = currentLanguage;
  renderProductDetail();
  renderCart();
  translateStaticText(currentLanguage);
  translateAttributes(currentLanguage);
  renderLanguageControls();
  setMenuButtonLabel(menu?.classList.contains("is-open"));
}

document.addEventListener("click", (event) => {
  const languageButton = event.target.closest("[data-language-button]");
  if (!languageButton) return;
  currentLanguage = languageButton.dataset.languageButton === "en" ? "en" : "es";
  writeLanguagePreference(currentLanguage);
  applyLanguage(currentLanguage);
});

document.addEventListener("click", (event) => {
  const button = event.target.closest("[data-add-to-cart]");
  if (!button) return;
  addToCart({
    id: button.dataset.id,
    name: button.dataset.name,
    price: Number(button.dataset.price),
    image: button.dataset.image,
  });
  const original = button.textContent;
  button.textContent = t("Añadido a la cesta");
  button.classList.add("is-added");
  window.setTimeout(() => {
    button.textContent = original;
    button.classList.remove("is-added");
  }, 1300);
});

if (menuToggle && menu) {
  menuToggle.addEventListener("click", () => {
    const isOpen = menu.classList.toggle("is-open");
    syncMenuAccessibility(isOpen);
  });

  menu.addEventListener("click", (event) => {
    if (event.target.closest("a")) closeMenu();
  });
}

if (cartView) {
  cartView.addEventListener("click", (event) => {
    const decrease = event.target.closest("[data-cart-decrease]");
    const increase = event.target.closest("[data-cart-increase]");
    const remove = event.target.closest("[data-cart-remove]");
    if (decrease) updateCartItem(decrease.dataset.cartDecrease, "decrease");
    if (increase) updateCartItem(increase.dataset.cartIncrease, "increase");
    if (remove) updateCartItem(remove.dataset.cartRemove, "remove");
  });

  cartView.addEventListener("submit", (event) => {
    const form = event.target.closest("[data-cart-payment-form]");
    if (!form) return;
    event.preventDefault();
    const cart = readCart();
    const note = form.querySelector("[data-form-note]");

    if (!cart.length) {
      if (note) note.textContent = t("Tu cesta está vacía. Agrega productos antes de continuar.");
      return;
    }

    const total = getCartTotal(cart);
    if (note) {
      note.textContent = `${t("Datos listos para pago. Total:")} ${formatCurrency(total)}. ${t("Falta conectar Openpay para completar la compra.")}`;
    }
  });
}

if (contactForm) {
  contactForm.addEventListener("submit", (event) => {
    event.preventDefault();
    const note = contactForm.querySelector("[data-form-note]");
    if (note) note.textContent = t("Gracias. Pronto nos pondremos en contacto.");
    contactForm.reset();
  });
}

document.querySelectorAll(".site-nav a").forEach((link) => {
  const current = location.pathname.split("/").pop() || "index.html";
  const href = link.getAttribute("href");
  if (href === current) link.classList.add("is-active");
});

if (cookieBanner) {
  let accepted = "";
  try {
    accepted = localStorage.getItem("okiyama-cookies");
  } catch {
    accepted = "";
  }
  cookieBanner.hidden = accepted === "yes";
  cookieBanner.addEventListener("click", (event) => {
    if (event.target.closest("button")) {
      try {
        localStorage.setItem("okiyama-cookies", "yes");
      } catch {
        // Cookie preference persistence can be unavailable in some file:// contexts.
      }
      cookieBanner.hidden = true;
    }
  });
}

document.querySelectorAll("[data-cookie-open]").forEach((link) => {
  link.addEventListener("click", (event) => {
    event.preventDefault();
    if (cookieBanner) cookieBanner.hidden = false;
  });
});

window.addEventListener("scroll", setHeaderState, { passive: true });
window.addEventListener("resize", () => {
  if (window.innerWidth > 900) closeMenu();
  syncMenuAccessibility(menu?.classList.contains("is-open"));
});

setHeaderState();
updateCartCount();
applyLanguage(currentLanguage);
syncMenuAccessibility(menu?.classList.contains("is-open"));
