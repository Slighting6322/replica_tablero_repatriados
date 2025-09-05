class HeaderGob extends HTMLElement {
  connectedCallback() {
    this.innerHTML = `
      <header class="barra-logo">
        <div class="logo-titulo">
          <img src="images/Horizontal Gobierno de México_01.png" alt="Logo del gobierno" style="height: 80px;" />
        </div>
        <nav class="header-nav">
          <a href="#home">Ocupación por Centro de Atención</a>
          <a href="#origen">Origen y Destino</a>
        </nav>
      </header>
    `;
  }
}
customElements.define('header-gob', HeaderGob);