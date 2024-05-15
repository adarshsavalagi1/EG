import React, { useState } from 'react';
import Navbar from '../components/Navbar';
import axios from 'axios';
import toast, { Toaster } from 'react-hot-toast';

export default function Homepage() {

    const [visible, setVisible] = useState(false);
    const [code, setCode] = useState('');
    const [summary, setSummary] = useState({
        data_structure: [],
        data_tables: [],
        variables_with_data: [],
        variables_without_data: [],
        functions: []

    });
    const [genCode, setGenCode] = useState('');
    const handleSubmit = async () => {
        try {

            const response = await axios.post('http://localhost:8000/summarize', { code: code });
            console.log(response.data);
            setSummary(
                response.data.summary
            );
            setGenCode(response.data.summary.code);
            toast.success('Code summarized Successfully');
            setVisible(true);

        } catch (error) {
            console.log(error);
            toast.error('Error in converting code');
        }
    }
    return (
        <>
            <Navbar />
            <Toaster />
            <div className='w-9/12 mx-auto'>

                <label htmlFor="message" className="block my-4 font-bold text-center  text-3xl">Cobol Code Converter</label>
                <textarea id="message" onChange={(e) => { setCode(e.target.value) }} value={code} rows="10" className="block p-2.5 w-full text-sm  bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500  dark:border-gray-600 dark:placeholder-gray-400  dark:focus:ring-blue-500 dark:focus:border-blue-500" placeholder="Paste your code here..."></textarea>
                <button type="button" onClick={handleSubmit} className="mt-5 text-white bg-white border border-gray-300 focus:outline-none hover:bg-gray-100 focus:ring-4 focus:ring-gray-100 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-gray-800  dark:border-gray-600 dark:hover:bg-gray-700 dark:hover:border-gray-600 dark:focus:ring-gray-700">get summary</button>
                <div className={` mb-2 ${visible ? 'block' : 'hidden'}`}>
                    <h1 className='text-2xl font-semibold'>Summary</h1>
                    <div className='w-11/12 mx-auto'>
                        {summary.variables_without_data &&
                            <>
                                <p className='text-lg font-semibold my-3'> Variables without Initialised: </p>
                                <ul className='list mx-auto'>
                                    {summary.variables_without_data.filter((item, index, self) => self.indexOf(item) === index).map((item, index) =>
                                        <li key={index} className='list list-disc ml-5'>{item}</li>
                                    )}
                                </ul>
                            </>
                        }
                        {summary.variables_with_data &&
                            <>
                                <p className='text-lg font-semibold my-3'> Varibles with Initialised: </p>
                                <ul className='list mx-auto'>{summary.variables_with_data.map((item, index) => { return <li key={index} className='list list-disc ml-5'>{item[0]}={item[1]}</li> })}</ul>
                            </>
                        }
                        {
                            summary.data_structure &&
                            <>
                                <p className='text-lg font-semibold my-3'>  Data Structure: </p>
                                <ul className='list mx-auto'>{summary.data_structure.map((item, index) => { return <li key={index} className='list list-disc ml-5 mt-3'>{item}</li> })}</ul>
                            </>
                        }
                        {
                            summary.data_tables &&
                            <>
                                <p className='text-lg font-semibold my-3'>  Data Tables: </p>
                                <ul className='list-disc mx-auto ml-5'>
                                    {Object.keys(summary.data_tables).map((key, index) => (
                                        <li key={index}>
                                            {key}: <ol>{summary.data_tables[key].map((item, index) => (
                                                <li className=' ml-10' key={index}>{item[0]} : {item[1]}</li>
                                            ))}</ol>
                                        </li>
                                    ))}
                                </ul>
                            </>
                        }
                        <hr />
                        {
                            summary.functions && <>
                                <p className='text-lg font-semibold my-3'>  Procedures: </p>
                                {summary.functions.map((item, index) => <ol key={index} className='list list-decimal mx-auto ml-5 mt-3'>
                                    <h3>Procedure {index + 1}</h3>
                                    {
                                        item.map((subitem, index) => (
                                            <li key={index} className=' ml-10'>{subitem}</li>
                                        ))
                                    }
                                </ol>)}
                            </>
                        }
                    </div>
                    <h1 className='text-2xl font-semibold'>Code Output</h1>
                    <div id="message" className="relative">
                        <textarea rows="65" value={genCode} readOnly className="resize-none block p-2.5 w-full text-sm  bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500  dark:border-gray-600 dark:placeholder-gray-400  dark:focus:ring-blue-500 dark:focus:border-blue-500"></textarea>
                    </div>
                </div>
            </div>
        </>
    );
}
