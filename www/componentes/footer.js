class FooterGob extends HTMLElement {
  connectedCallback() {
    this.innerHTML = `
      <footer class="footer-gob">
        <div class="footer-container">
          <div class="footer-logo">
            <img src="images/2025_IMAGOTIPO_HORIZONTAL PARA FONDO OBSCURO.png" alt="Logo Gob" style="height: 100px;" />
          </div>
        </div>
      </footer>
    `;
  }
}
customElements.define('footer-gob', FooterGob);