class HeaderGob extends HTMLElement {
  connectedCallback() {
    this.innerHTML = `
      <header class="barra-logo">
        <div class="logo-titulo">
          <img src="images/Horizontal Gobierno de México_01.png" alt="Logo del gobierno" style="height: 80px;" />
        </div>
        <nav class="header-nav">
          <a href="#home" class="nav-link">Ocupación por Centro de Atención</a>
          <a href="#origen" class="nav-link">Origen y Destino</a>
        </nav>
      </header>
    `;

    // Añadir comportamiento para marcar la pestaña activa
    requestAnimationFrame(() => {
      const links = this.querySelectorAll('.header-nav .nav-link');
      function setActive(hash) {
        links.forEach(a => {
          a.classList.toggle('active', a.getAttribute('href') === '#' + hash);
        });
      }

      // click handlers: el router global ya maneja showSection; aquí sólo resaltar
      links.forEach(a => a.addEventListener('click', (ev) => {
        // allow router to handle navigation
        const h = a.getAttribute('href') || '';
        if (h.startsWith('#')) {
          setActive(h.replace('#',''));
        }
      }));

      // on load / hashchange
      window.addEventListener('hashchange', () => setActive(location.hash.replace('#','')));
      setActive(location.hash.replace('#','') || 'home');
    });
  }
}
customElements.define('header-gob', HeaderGob);