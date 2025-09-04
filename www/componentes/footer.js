class FooterGob extends HTMLElement {
  connectedCallback() {
    this.innerHTML = `
      <footer class="footer-gob">
        <div class="footer-container">
          <div class="footer-logo">
            <img src="imagenes/Logo Gobierno 2025.svg" alt="Logo Gob" />
          </div>
          <div class="footer-col">
            <h4>Enlaces</h4>
            <ul>
              <li><a href="https://datos.gob.mx" target="_blank" rel="noopener">Datos</a></li>
              <li><a href="https://www.gob.mx/publicaciones" target="_blank" rel="noopener">Publicaciones</a></li>
              <li><a href="http://portaltransparencia.gob.mx" target="_blank" rel="noopener">Portal de Obligaciones de Transparencia</a></li>
              <li><a href="https://consultapublicamx.plataformadetransparencia.org.mx/vut-web/faces/view/consultaPublica.xhtml?idEntidad=MzM=&idOrgano=MzM=&idSujetoObligado=Nzg=#inicio" target="_blank" rel="noopener">PNT</a></li>
              <li><a href="https://alertadores.buengobierno.gob.mx" target="_blank" rel="noopener">Alerta</a></li>
              <li><a href="https://sidec.buengobierno.gob.mx" target="_blank" rel="noopener">Denuncia</a></li>
            </ul>
          </div>
          <div class="footer-col">
            <h4>¿Qué es gob.mx?</h4>
            <p>
              Es el portal único de trámites, información y participación ciudadana.<br>
              <a href="https://www.gob.mx/que-es-gobmx">Leer más</a>
            </p>
            <ul>
              <li><a href="https://www.gob.mx/accesibilidad" class="declaracion-accesibilidad">Declaración de Accesibilidad</a></li>
              <li><a href="https://www.gob.mx/aviso_de_privacidad" class="aviso-privacidad">Aviso de privacidad</a></li>
              <li><a href="https://www.gob.mx/privacidadsimplificado" class="aviso-privacidad-simplificado">Aviso de privacidad simplificado</a></li>
              <li><a href="https://www.gob.mx/terminos" class="terminos-condiciones">Términos y Condiciones</a></li>
              <li><a href="https://www.gob.mx/terminos#medidas-seguridad-informacion" class="politica-seguridad">Política de seguridad</a></li>
              <li><a href="https://www.ordenjuridico.gob.mx" target="_blank" rel="noopener">Marco jurídico</a></li>
            </ul>
          </div>
          <div class="footer-col">
            <a href="https://sidec.buengobierno.gob.mx" class="denuncia">Denuncia contra servidores públicos</a>
            <h4>Síguenos en</h4>
            <div class="redes">
              <a href="https://web.facebook.com/gobmexico?_rdc=1&_rdr#" target="_blank" rel="noopener">
                <img src="imagenes/fb.svg" alt="Facebook" width="32" height="32" />
              </a>
              <a href="https://x.com/GobiernoMX" target="_blank" rel="noopener">
                <img src="imagenes/twitter.svg" alt="Twitter" width="32" height="32" />
              </a>
            </div>
          </div>
        </div>
      </footer>
    `;
  }
}
customElements.define('footer-gob', FooterGob);