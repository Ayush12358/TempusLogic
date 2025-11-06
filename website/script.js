function parseCsv(text) {
    const rows = text.trim().split(/\r?\n/);
    return rows.map((row) => row.split(','));
}

function renderTable(data) {
    const rows = data.slice(1); // skip CSV header row which duplicates defined headers
    if (!rows.length) {
        return '<p>No results found. Run the benchmark to populate rankings.</p>';
    }

    const headers = ['Model', 'GSM8K', 'Dyad/Triad', 'Coding Test', 'Average', 'Timestamp'];
    const tableHead = headers.map((heading) => `<th>${heading}</th>`).join('');
    const tableRows = rows
        .map((cols) => {
            const padded = [...cols];
            while (padded.length < headers.length) {
                padded.push('');
            }
            const cells = padded
                .slice(0, headers.length)
                .map((value) => `<td>${value}</td>`)
                .join('');
            return `<tr>${cells}</tr>`;
        })
        .join('');

    return `<table><thead><tr>${tableHead}</tr></thead><tbody>${tableRows}</tbody></table>`;
}

async function loadRankings() {
    const container = document.getElementById('rankings');
    if (!container) {
        return;
    }

    container.innerHTML = '<p>Loading rankings...</p>';

    try {
        const response = await fetch('../rankings.csv', { cache: 'no-store' });
        if (!response.ok) {
            throw new Error(`HTTP ${response.status}`);
        }
        const text = await response.text();
        if (!text.trim()) {
            container.innerHTML = '<p>No results yet. Run the benchmark to generate scores.</p>';
            return;
        }
        const data = parseCsv(text);
        container.innerHTML = renderTable(data);
    } catch (error) {
        container.innerHTML = `<p class="error">Unable to load rankings (${error.message}).</p>`;
    }
}

document.addEventListener('DOMContentLoaded', () => {
    const refreshButton = document.getElementById('refresh');
    if (refreshButton) {
        refreshButton.addEventListener('click', loadRankings);
    }
    loadRankings();
});
