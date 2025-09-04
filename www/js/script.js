document.addEventListener('DOMContentLoaded', function() {
  
  // Cargar datos para los KPI
  fetch('datos/kpi-box.json')
    .then(response => response.json())
    .then(kpis => {
      const contenedor = document.querySelector('.contenedor-kpis');
      if (!contenedor) return;
      kpis.forEach(kpi => {
        const el = document.createElement('kpi-box');
        el.setAttribute('valor', kpi.valor);
        el.setAttribute('extra', kpi.extra);
        el.setAttribute('anio', kpi.anio);
        el.setAttribute('porcentaje', kpi.porcentaje);
        contenedor.appendChild(el);
      });
    })
    .catch(err => console.error('Error cargando KPIs:', err));


// Cargar datos para la gráfica de línea
fetch('datos/grafica-linea.json')
  .then(resp => resp.json())
  .then(datos => {
    const graficaLinea = document.querySelector('grafica-linea');
    if (graficaLinea) {
      graficaLinea.datos = datos;
    }
  });

// Cargar datos para la gráfica escalonada
fetch('datos/grafica-escalonada.json')
  .then(resp => resp.json())
  .then(datos => {
    const grafica = document.querySelector('grafica-escalonada');
    grafica.datos = datos;
  });

fetch('datos/indicadore-box.json')
  .then(response => response.json())
  .then(indicadores => {
    const contenedor = document.querySelector('.indicadores-resumen');
    indicadores.forEach(indicador => {
      const el = document.createElement('indicador-box');
      el.setAttribute('etiqueta', indicador.etiqueta);
      el.setAttribute('color', indicador.color);
      el.setAttribute('valor', indicador.valor);
      el.setAttribute('porcentaje', indicador.porcentaje);
      el.setAttribute('cantidad', indicador.cantidad);
      el.setAttribute('promedio', indicador.promedio);
      contenedor.appendChild(el);
    });
  });

// Cargar y renderizar los kpi-box de aduanas
async function cargarKpiAduanas() {
  const container = document.getElementById('kpi-aduanas-table');
  if (!container) return;
  container.innerHTML = '<div>Cargando...</div>';
  try {
    const response = await fetch('datos/kpi-box-aduanas.json');
    const data = await response.json();
    container.innerHTML = '';
    // Un solo contenedor flex
    const row = document.createElement('div');
    row.className = 'kpi-row';
    container.appendChild(row);

    data.forEach((kpi) => {
      const kpiBox = document.createElement('kpi-box');
      kpiBox.setAttribute('valor', kpi.valor);
      if (kpi.extra) kpiBox.setAttribute('extra', kpi.extra);
      if (kpi.extra2) kpiBox.setAttribute('extra2', kpi.extra2);
      if (kpi.anio) kpiBox.setAttribute('anio', kpi.anio);
      if (kpi.anio2) kpiBox.setAttribute('anio2', kpi.anio2);
      if (kpi.porcentaje) kpiBox.setAttribute('porcentaje', kpi.porcentaje);
      row.appendChild(kpiBox);
    });
  } catch (e) {
    container.innerHTML = '<div>Error al cargar los datos</div>';
  }
}

fetch('datos/matriz-kpi-box.json')
  .then(resp => resp.json())
  .then(data => {
    const matriz = document.querySelector('kpi-matrix');
    if (matriz) matriz.datos = data;
  });


const matriz = document.querySelector('kpi-matrix');
matriz.addEventListener('kpi-selected', async (e) => {
  // Aquí puedes decidir qué datos cargar según el KPI seleccionado
  const graficaLinea = matriz.shadowRoot.querySelector('#grafica-linea-matrix');
  const graficaEscalonada = matriz.shadowRoot.querySelector('#grafica-escalonada-matrix');

  // Ejemplo: cargar datos genéricos, o personalizados según e.detail
  if (graficaLinea) {
    const datosLinea = await fetch('datos/grafica-linea.json').then(r => r.json());
    graficaLinea.datos = datosLinea;
  }
  if (graficaEscalonada) {
    const datosEscalonada = await fetch('datos/grafica-escalonada.json').then(r => r.json());
    graficaEscalonada.datos = datosEscalonada;
  }
});

// Función para alternar el dropdown
  function toggleDropdown(button, contentId) {
    const content = document.getElementById(contentId);
    if (!content) return;
    if (content.classList.contains('visible')) {
      content.classList.remove('visible');
      button.classList.remove('open');
    } else {
      content.classList.add('visible');
      button.classList.add('open');
    }
  }

  const toggleBtn = document.querySelector('.dropdown-toggle');
  const contenido = document.getElementById('contenido-resumen');
  if (toggleBtn && contenido) {
    toggleBtn.addEventListener('click', function(e) {
      e.stopPropagation();
      contenido.classList.toggle('visible');
      toggleBtn.classList.toggle('open');
    });
    // Cierra el dropdown si haces click fuera
    document.addEventListener('click', function(e) {
      if (!contenido.contains(e.target) && !toggleBtn.contains(e.target)) {
        contenido.classList.remove('visible');
        toggleBtn.classList.remove('open');
      }
    });
  }
});
